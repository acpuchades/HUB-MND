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
        n_assessments = sum(!is.na(alsfrs_total)),
        time_from_onset = max(fecha_visita) - first(fecha_inicio_clinica),
        alsfrs_slope = last(alsfrs_total) / (time_from_onset %/% dmonths(1)),
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
    )

generic_survivals <- ufmn_patients %>%
    inner_join(ufmn_clinical, by = "pid") %>%
    left_join(ufmn_followups, by = "pid") %>%
    group_by(pid) %>%
    summarize(
        time_of_onset = first(fecha_inicio_clinica),
        vital_status = case_when(
            first(exitus) == TRUE ~ "dead",
            TRUE ~ "alive_or_missing",
        ),
        last_vital_status = case_when(
            vital_status == "dead" ~ first(fecha_exitus),
            TRUE ~ max(fecha_visita),
        ),
        time_until_death_or_last_followup =
            last_vital_status - first(fecha_inicio_clinica),
        survival_category = case_when(
            vital_status == "dead" & time_until_death_or_last_followup < generic_survivals_fast_progression_threshold ~ "fast",
            vital_status == "dead" &
                time_until_death_or_last_followup >= generic_survivals_fast_progression_threshold &
                time_until_death_or_last_followup < generic_survivals_slow_progression_threshold ~ "average",
            time_until_death_or_last_followup >= generic_survivals_slow_progression_threshold ~ "slow",
        )
    ) %>%
    drop_na(time_until_death_or_last_followup)

generic_export <- function(path, anonimize_data = TRUE) {
    if (!file.exists(dirname(path))) {
        dir.create(dirname(path), recursive = TRUE)
    }

    write_xlsx(list(
        "slopes" = generic_slopes,
        "survivals" = generic_survivals
    ), with_ext(path, ".xlsx"))
}
