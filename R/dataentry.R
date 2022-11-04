library(dplyr)

dataentry_data_path <- "data/dataentry-2022_11_04.xlsx"

dataentry_visits <-
    readxl::read_excel(dataentry_data_path, range = cellranger::cell_cols("B:F")) %>%
    setNames(c("anterior", "primera_visita", "nhc", "paciente", "fecha_visita")) %>%
    extract(
        paciente,
        into = c("apellidos", "nombre", "sexo", "edad"),
        r"(^([A-ZÑ\s]+) ?, ([A-ZÑ\s]+) \(([HD]),(\d+)A\)$)", convert = TRUE
    ) %>%
    mutate(
        across(fecha_visita, as.Date),
        sexo = recode(sexo, H = "Hombre", D = "Mujer"),
    ) %>%
    select(nhc, nombre, apellidos, sexo, edad, fecha_visita)
