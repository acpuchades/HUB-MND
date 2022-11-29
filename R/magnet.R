source("R/ufmn.R")

magnet_inclusion_period_start <- lubridate::dmy("01/01/2022")
magnet_inclusion_period_end <- magnet_inclusion_period_start + dmonths(12)

magnet_patients <- ufmn_patients %>%
    dplyr::left_join(ufmn_clinical, by = "pid") %>%
    dplyr::filter(
        is.na(fecha_exitus) | (fecha_exitus > magnet_inclusion_period_end),
        between(
            fecha_diagnostico,
            magnet_inclusion_period_start,
            magnet_inclusion_period_end
        )
    ) %>%
    dplyr::select(
        pid,
        nhc,
        nombre,
        primer_apellido,
        segundo_apellido,
        fecha_diagnostico,
        fenotipo_al_diagnostico
    ) %>%
    dplyr::arrange(fecha_diagnostico)
