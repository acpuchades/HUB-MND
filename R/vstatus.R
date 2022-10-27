source("R/anonimize.R")
source("R/ufmn.R")

vstatus_alive <- ufmn_patients %>%
    dplyr::filter(
        exitus != TRUE & is.na(fecha_exitus),
    ) %>%
    dplyr::left_join(
        ufmn_followups %>%
            group_by(pid) %>%
            arrange(desc(fecha_visita), .by_group = TRUE) %>%
            summarize(ultima_visita = first(fecha_visita)),
        by = "pid"
    ) %>%
    select(
        pid, nhc, cip, nombre, primer_apellido, segundo_apellido, ultima_visita
    )

vstatus_export <- function(path) {
    if (!file.exists(path)) {
        dir.create(path, recursive = TRUE)
    }
    readr::write_csv(vstatus_alive, file.path(path, "vstatus-alive.csv"))
}
