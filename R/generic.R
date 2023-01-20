library(lubridate)
library(writexl)
library(xfun)

source("R/ufmn.R")

generic_slopes_min_followup_visits <- 3
generic_slopes_min_followup_time <- dyears(1)
generic_slopes_slow_progression_threshold <- 0.5
generic_slopes_fast_progression_threshold <- 1.5

generic_survivals_fast_progression_threshold <- dmonths(15)
generic_survivals_slow_progression_threshold <- dmonths(100)

generic_slopes <- ufmn_clinical %>%
    left_join(ufmn_functional, by = "pid") %>%
    group_by(pid) %>%
    arrange(fecha_visita, .by_group = TRUE) %>%
    summarize(
        alsfrs_baseline = first(alsfrs_total),
        alsfrs_last = last(alsfrs_total),
        n_assessments = sum(!is.na(alsfrs_total)),
        time_from_onset = max(fecha_visita) - first(fecha_inicio_clinica),
        alsfrs_slope = (48 - alsfrs_last) / (time_from_onset %/% dmonths(1)),
        slope_category = case_when(
            alsfrs_slope < generic_slopes_slow_progression_threshold ~ "slow",
            between(
                alsfrs_slope, generic_slopes_slow_progression_threshold,
                generic_slopes_fast_progression_threshold
            ) ~ "average",
            alsfrs_slope >= generic_slopes_fast_progression_threshold ~ "fast"
        )
    ) %>%
    filter(
        n_assessments >= generic_slopes_min_followup_visits,
        time_from_onset >= generic_slopes_min_followup_time
    ) %>%
    left_join(
        ufmn_patients %>% select(pid, nhc, cip),
        by = "pid"
    ) %>%
    relocate(nhc:cip, .after = pid)

generic_survivals <- ufmn_patients %>%
    left_join(ufmn_clinical, by = "pid") %>%
    left_join(
        ufmn_followups %>% select(pid, fecha_visita),
        by = "pid"
    ) %>%
    left_join(ufmn_functional, by = c("pid", "fecha_visita")) %>%
    group_by(pid) %>%
    mutate(
        ventilation_start_date = if_else(disnea <= 1, fecha_visita, NA_Date_)
    ) %>%
    summarize(
        time_of_onset = first(fecha_inicio_clinica),
        vital_status = case_when(
            first(exitus) == TRUE ~ "dead",
            TRUE ~ "alive_or_missing",
        ),
        time_of_ventilation_support = suppressWarnings(
            min(ventilation_start_date, na.rm = TRUE)
        ),
        time_of_last_vital_status = case_when(
            vital_status == "dead" ~ first(fecha_exitus),
            TRUE ~ max(fecha_visita),
        )
    ) %>%
    mutate(
        time_of_ventilation_support = if_else(
            is.infinite(time_of_ventilation_support),
            NA_Date_, time_of_ventilation_support
        ),
        endpoint_date = case_when(
            !is.na(time_of_ventilation_support) ~ time_of_ventilation_support,
            vital_status == "dead" ~ time_of_last_vital_status
        ),
        observation_time = time_of_last_vital_status - time_of_onset,
        time_to_endpoint = endpoint_date - time_of_onset,
        survival_category = case_when(
            !is.na(endpoint_date) & time_to_endpoint < generic_survivals_fast_progression_threshold ~ "fast",
            !is.na(endpoint_date) & time_to_endpoint >= generic_survivals_fast_progression_threshold &
                time_to_endpoint < generic_survivals_slow_progression_threshold ~ "average",
            observation_time >= generic_survivals_slow_progression_threshold ~ "slow",
        )
    ) %>%
    drop_na(time_of_last_vital_status) %>%
    left_join(
        ufmn_patients %>% select(pid, nhc, cip),
        by = "pid"
    ) %>%
    relocate(nhc:cip, .after = pid)

generic_export <- function(path, anonimize_data = TRUE) {
    if (!file.exists(dirname(path))) {
        dir.create(dirname(path), recursive = TRUE)
    }

    write_xlsx(list(
        "slopes" = generic_slopes,
        "survivals" = generic_survivals
    ), with_ext(path, ".xlsx"))
}
