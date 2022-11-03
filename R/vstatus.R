source("R/anonimize.R")
source("R/ufmn.R")
source("R/dataentry.R")

vstatus_all <- ufmn_patients %>%
    mutate(apellidos = paste(primer_apellido, segundo_apellido)) %>%
    left_join(
        ufmn_followups %>%
            group_by(pid) %>%
            summarize(ultima_visita = max(fecha_visita)),
        by = "pid"
    ) %>%
    full_join(
        dataentry_visits %>%
            group_by(nhc) %>%
            mutate(ultima_visita = max(fecha_visita)) %>%
            filter(fecha_visita == ultima_visita),
        by = "nhc", suffix = c("_ufmn", "_dataentry")
    ) %>%
    mutate(
        nombre = coalesce(nombre_ufmn, nombre_dataentry),
        apellidos = coalesce(apellidos_ufmn, apellidos_dataentry),
        ultima_visita = pmax(ultima_visita_ufmn, ultima_visita_dataentry, na.rm = TRUE)
    ) %>%
    select(pid, nhc, nombre, apellidos, exitus, fecha_exitus, ultima_visita)

vstatus_alive <- vstatus_all %>%
    filter(exitus %in% c(FALSE, NA), is.na(fecha_exitus)) %>%
    select(-c(exitus, fecha_exitus))

vstatus_export <- function(path) {
    if (!file.exists(path)) {
        dir.create(path, recursive = TRUE)
    }

    readr::write_csv(vstatus_all, file.path(path, "vstatus-all.csv"))
    readr::write_csv(vstatus_alive, file.path(path, "vstatus-alive.csv"))
}
