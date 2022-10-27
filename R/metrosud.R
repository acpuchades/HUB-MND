library(dplyr)
library(magrittr)
library(readr)
library(readxl)
library(stringr)
library(tibble)
library(tidyr)

metrosud_cronic_data_path <- "data/metrosud-cronic-2022_08_26.xls"
metrosud_farmacia_data_path <- "data/metrosud-farmacia-2022_08_26.xls"
metrosud_problemas_data_path <- "data/metrosud-problemes-2022_08_26.xls"
metrosud_variables_data_path <- "data/metrosud-variables-2022_08_26.xlsx"
metrosud_visitas_data_path <- "data/metrosud-visites-2022_08_26.xls"

metrosud_cronic <- read_excel(metrosud_cronic_data_path) %>%
  rename(
    cip = CIP,
    pcc_maca = CRONIC,
  )

metrosud_farmacia <- read_excel(metrosud_farmacia_data_path) %>%
  rename(
    cip = CIP,
    fecha_inicio = DATA_INICI,
    fecha_fin = DAT_FI,
    cod_producto = PF_CODI,
    desc_producto = PF_DESC,
    posologia = POSOLOGIA,
    cod_principio = PASCODI,
    desc_principio = PAS_DESC,
    frecuencia = FREQUENCIA,
  ) %>%
  mutate(
    across(starts_with("fecha_"), as.Date, origin = "1900-01-01"),
  )

metrosud_problemas <- read_excel(metrosud_problemas_data_path) %>%
  rename(
    cip = CIP,
    cod_problema = CODI_PROBLEMA,
    desc_problema = DESCRIPCIO_PROBLEMA,
    fecha_problema = DATA_PROBLEMA,
  ) %>%
  mutate(
    across(starts_with("fecha_"), as.Date, origin = "1900-01-01"),
    cod_problema = str_replace(cod_problema, "C01-", ""),
  )

metrosud_variables <- read_excel(metrosud_variables_data_path) %>%
  rename(
    cip = CIP,
    cod_variable = CODI_VARIABLE,
    desc_variable = DESCRIPCIO_VARIABLE,
    valor = VALOR_VARIABLE,
    fecha_registro = DATA_VARIABLE,
  ) %>%
  mutate(
    across(starts_with("fecha_"), as.Date, origin = "1900-01-01"),
    desc_variable = recode(desc_variable,
      `INDEX DE BARTHEL: 55 O MENYS` = "Index de Barthel",
      `Escala de BARTHEL. Activitat vida diària` = "Index de Barthel",
      `DEMÈNCIA Y DETERIORAMENT COGNITIU (TEST COGNITIU DE PFEIFFER: 5 O MÉS ERRORS)` = "Index de Pfeiffer",
      `Test de PFEIFFER. Val. Mental.` = "Index de Pfeiffer",
      `Test de LAWTON-BRODY` = "Index de Lawton-Brody",
      `IMC - Índex de Massa Corporal` = "Index de Massa Corporal",
    )
  )

metrosud_visitas <- read_excel(metrosud_visitas_data_path) %>%
  rename(
    cip = CIP,
    fecha_visita = DATA_VISITA,
    cod_problema = PR_PRINCIPAL,
    desc_problema = PR_PRINCIPAL_DES,
  ) %>%
  mutate(
    across(starts_with("fecha_"), as.Date, origin = "1900-01-01"),
    cod_problema = str_replace(cod_problema, "C01-", ""),
  )


metrosud_export <- function(path, anonimize_data = TRUE) {
  if (!file.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  exports <- list(
    "cronic" = metrosud_cronic,
    "farmacia" = metrosud_farmacia,
    "problemas" = metrosud_problemas,
    "variables" = metrosud_variables,
    "visitas" = metrosud_visitas
  )

  for (key in names(exports)) {
    data <- exports[[key]]
    if (anonimize_data) {
      data <- anonimize(data)
    }
    write_csv(data, file.path(path, paste("metrosud-", key, ".csv", sep = "")))
  }
}