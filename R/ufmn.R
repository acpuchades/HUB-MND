library(dplyr)
library(magrittr)
library(purrr)
library(stringr)
library(stringi)
library(readr)
library(tidyr)

source("R/anonimize.R")

ufmn_data_path <- "data/ufmn-2023_01_18.sqlite"

ufmn_parse_na <- function(data, na_empty = FALSE) {
  if (na_empty) {
    data %<>% na_if("")
  }

  data %>%
    na_if("-") %>%
    na_if("NS/NC")
}

ufmn_parse_date <- function(data) {
  data %>%
    ufmn_parse_na(na_empty = TRUE) %>%
    as.Date(format = "%d-%m-%Y")
}

ufmn_parse_logical <- function(data, true, false) {
  case_when(
    data %in% true ~ TRUE,
    data %in% false ~ FALSE,
  )
}

ufmn_parse_factor <- function(data, ...) {
  data %>%
    ufmn_parse_na(na_empty = TRUE) %>%
    factor(...)
}

ufmn_parse_studies <- function(data) {
  data %>%
    recode(
      `No sabe leer ni escribir` = "Analfabeto",
      `Secundarios (ESO, BUP, COU, etc)` = "Secundarios",
      `FP (grado medio o superior)` = "Formacion profesional",
      Universidad = "Universitarios",
      Otro = "Otros"
    ) %>%
    ufmn_parse_factor(
      levels = c(
        "Otros",
        "Analfabeto",
        "Primarios incompletos",
        "Primarios completos",
        "Secundarios",
        "Formacion profesional",
        "Universitarios",
        "Doctorado"
      ),
      ordered = TRUE
    )
}

ufmn_parse_working_status <- function(data) {
  data %>%
    recode(
      Parado = "En paro (sin subsidio)",
      `Parado con subsidio | Prestación` = "En paro (con subsidio)",
      `Incapacitado (o con invalidez permanente)` = "Incapacitado",
      Otra = "Otros"
    ) %>%
    ufmn_parse_factor()
}

ufmn_parse_genetic_result <- function(data, ...) {
  data %>%
    ufmn_parse_factor(levels = c("Normal", "Alterado"), ...)
}

ufmn_parse_municipality <- function(data) {
  data %>%
    ufmn_parse_na() %>%
    str_replace("L'\\s+", "L'") %>%
    recode(
      `FERROL (2)` = "FERROL",
      `MOLÃƒÂ D'AVALL` = "MOLI D'AVALL",
      `PRAT DE LLOBREGAT` = "EL PRAT DE LLOBREGAT",
      `SANT JOAN DE VILATORRSANT JOAN DE VILASANT JOAN DE VILATORRADATORRADAADA` = "SANT JOAN DE VILATORRADA",
      `SANT SADURNÃ D'ANOIA` = "SANT SADURNI D'ANOIA",
      `SANT LLORENÃƒÆ’Ã¢â‚¬Â¡ D'HORTONS` = "SANT LLORENÇ D'HORTONS"
    ) %>%
    stri_trans_general(id = "Latin-ASCII")
}

ufmn_parse_cognitive <- function(data) {
  data %>%
    recode(
      `Deterioro Cognitivo Leve cognitivo (DCL cognitivo)` = "DCL-Cognitivo",
      `Deterioro Cognitivo Leve conductual (DCL conductual)` = "DCL-Conductual",
      `Deterioro Cognitivo Leve mixto (DCL mixto)` = "DCL-Mixto",
      `Demencia frontotemporal` = "DFT",
      `Demencia tipo alzheimer` = "DTA"
    ) %>%
    ufmn_parse_factor(levels = c(
      "Otros", "Normal",
      "DCL-Cognitivo", "DCL-Conductual",
      "DCL-Mixto", "DTA", "DFT"
    ))
}

ufmn_parse_phenotype <- function(data) {
  data %>%
    recode(
      `Atrofia Muscular Progresiva (AMP)` = "AMP",
      `Esclerosis Lateral Primaria (ELP)` = "ELP",
      `Parálisis bulbar progresiva` = "PBP",
      `Flail arm` = "Flail-Arm",
      `Flail leg` = "Flail-Leg",
      `Hemipléjica (Mills)` = "Hemiplejica",
      `Monomiélica` = "Monomielica",
      `Pseudopolineurítica` = "Pseudopolineuritica"
    ) %>%
    ufmn_parse_factor()
}

ufmn_parse_distribution <- function(data) {
  data %>%
    str_split("@", n = 3) %>%
    map_chr(\(xs)
    case_when(
      any(xs == "MMSS") ~ "MMSS",
      any(xs == "EESS") ~ "MMSS",
      any(xs == "MMII") ~ "MMII",
      any(xs == "EEII") ~ "MMII",
      any(xs == "AMBAS") ~ "MMSS+MMII",
      any(xs == "MMSS Y MMII") ~ "MMSS+MMII",
      any(xs == "AMBAS IZQUIERDA") ~ "MMSS+MMII",
      any(str_detect(xs, "GENERALIZAD[OA]")) ~ "MMSS+MMII",
      any(xs == "BULBAR") ~ "Bulbar",
      any(str_detect(xs, "RESPIRATORI[OA]")) ~ "Respiratoria",
    )) %>%
    ufmn_parse_factor(levels = c(
      "Bulbar",
      "Respiratoria",
      "MMSS",
      "MMII",
      "MMSS+MMII"
    ))
}

ufmn_parse_involvement <- function(data) {
  data %>%
    str_split("@", n = 3) %>%
    map_chr(\(xs)
    case_when(
      any(xs == "BMN") ~ "MNS+MNI",
      any(xs == "UMN->BMN") ~ "MNS",
      any(xs == "UMN") ~ "MNS",
      any(xs == "LMN") ~ "MNI",
    )) %>%
    ufmn_parse_factor(levels = c("MNS", "MNI", "MNS+MNI"))
}

ufmn_parse_predominance <- function(data) {
  data %>%
    str_split("@", n = 3) %>%
    map_chr(\(xs) {
      xs <- rev(xs) # afectación @ predominio @ debilidad
      case_when(
        any(xs == "PREDOMINIO UMN") ~ "MNS",
        any(xs == "PREDOMINIO LMN") ~ "MNI",
        any(xs == "NINGUN PREDOMINIO") ~ "Ninguno",
        any(xs == "NINGUN PREDOMINIO+EXTRAPIRAMIDAL") ~ "Ninguno",
        any(xs == "PREDOMINIO LMN EN MMSS y PREDOMINIO UMN EN MMSS") ~ "Ninguno",
        any(xs == "UMN") ~ "MNS",
        any(xs == "LMN") ~ "MNI",
      )
    }) %>%
    ufmn_parse_factor(levels = c("Ninguno", "MNS", "MNI"))
}

ufmn_parse_resultado_estudio_atxn2 <- function(data) {
  atxn2_re <- regex("\\bATXN2\\b", ignore_case = TRUE)
  normal_re <- regex("\\bNORMAL\\b", ignore_case = TRUE)
  intermediate_re <- regex("\\bINTERMEDI[AO]?\\b", ignore_case = TRUE)

  data %>%
    str_split("@", n = 3) %>%
    map_chr(\(xs) case_when(
      any(str_detect(xs, atxn2_re) & str_detect(xs, normal_re)) ~ "Normal",
      any(str_detect(xs, atxn2_re) & str_detect(xs, intermediate_re)) ~ "Alterado"
    )) %>%
    ufmn_parse_genetic_result()
}

ufmn_parse_resultado_estudio_kennedy <- function(data) {
  kennedy_re <- regex("\\bKENNEDY\\b", ignore_case = TRUE)
  normal_re <- regex("\\bNORMAL\\b", ignore_case = TRUE)
  positive_re <- regex("\\bPOSITIVO\\b", ignore_case = TRUE)

  data %>%
    map_chr(\(xs) case_when(
      any(str_detect(xs, kennedy_re) & str_detect(xs, normal_re)) ~ "Normal",
      any(str_detect(xs, kennedy_re) & str_detect(xs, positive_re)) ~ "Alterado",
    )) %>%
    ufmn_parse_genetic_result()
}

ufmn_parse_dysphagia <- function(data) {
  data %>%
    recode(
      `Sí sólidos` = "Solidos",
      `Sí líquidos` = "Liquidos",
      `Sí líquidos y sólidos` = "Mixta"
    ) %>%
    ufmn_parse_factor(levels = c("No", "Solidos", "Liquidos", "Mixta"))
}

ufmn_parse_peg_usage <- function(data) {
  data %>%
    recode(
      HidratacionMeditacion = "Hidratacion + Medicacion",
      `HidratacionMedicaciónNutricionParcial` = "Hidratacion + Medicacion + Nutricion parcial",
      NutricionCompleta = "Nutricion completa"
    ) %>%
    ufmn_parse_factor(levels = c(
      "Hidratacion",
      "Hidratacion + Medicacion",
      "Hidratacion + Medicacion + Nutricion parcial",
      "Nutricion completa"
    ), ordered = TRUE)
}

anonimize.ufmn <- function(data) {
  data %>% select(-any_of(c(
    "nombre",
    "primer_apellido",
    "segundo_apellido",
    "dni",
    "cip",
    "direccion",
    "telefono_fijo",
    "telefono_movil"
  )))
}

ufmn_db <- DBI::dbConnect(RSQLite::SQLite(), ufmn_data_path)

ufmn_patients <- DBI::dbReadTable(ufmn_db, "pacientes") %>%
  select(!c(id, created_datetime:updated_datetime)) %>%
  rename(
    situacion_laboral_al_inicio = situacion_laboral_actual,
    situacion_laboral_al_inicio_otro = situacion_laboral_actual_otra,
    ultima_ocupacion_al_inicio = ultima_ocupacion,
    estudios_otros = estudios_otro_cual
  ) %>%
  mutate(
    across(everything(), ufmn_parse_na),
    across(nhc, parse_integer),
    fecha_nacimiento = ufmn_parse_date(fecha_nacimiento),
    sexo = ufmn_parse_factor(sexo),
    exitus = ufmn_parse_logical(exitus, true = "Sí", false = "No"),
    fecha_exitus = ufmn_parse_date(fecha_exitus),
    estudios = ufmn_parse_studies(estudios),
    situacion_laboral_al_inicio = ufmn_parse_working_status(situacion_laboral_al_inicio),
    municipio_residencia = ufmn_parse_municipality(municipio_residencia)
  ) %>%
  relocate(cip, .after = nhc) %>%
  structure(class = c("ufmn", class(.)))

ufmn_clinical <- DBI::dbReadTable(ufmn_db, "datos_clinicos") %>%
  select(!c(estudio_genetico_c9:estudio_genetico_sod1, created_datetime:updated_datetime)) %>%
  rename(
    fecha_primera_visita = fecha_visita_datos_clinicos,
    fecha_diagnostico = fecha_diagnostico_ELA,
    antecedentes_otros = otros_antecedentes_de_interes,
    historia_familiar_otros = historia_familiar_cual,
    historia_familiar_alzheimer_grado = historia_familiar_alzheimer_quien,
    historia_familiar_parkinson_grado = historia_familiar_parkinson_quien,
    historia_familiar_motoneurona_grado = historia_familiar_motoneurona_quien,
    resultado_estudio_cognitivo = estudio_cognitivo,
    resultado_estudio_cognitivo_otros = estudio_cognitivo_otros,
    estudio_gen_c9 = resultado_estudio_c9,
    estudio_gen_sod1 = resultado_estudio_sod1,
    estudio_gen_otros = estudio_genetico_otro
  ) %>%
  mutate(across(everything(), ufmn_parse_na),
    across(starts_with("fecha"), ufmn_parse_date),
    across(c(
      historia_familiar,
      historia_familiar_motoneurona,
      historia_familiar_alzheimer,
      historia_familiar_parkinson,
      deterioro_cognitivo,
      riluzol
    ), ufmn_parse_logical, true = "Sí", false = "No"),
    fumador = ufmn_parse_factor(fumador),
    resultado_estudio_cognitivo = ufmn_parse_cognitive(resultado_estudio_cognitivo),
    fenotipo_al_diagnostico = ufmn_parse_phenotype(fenotipo_al_diagnostico),
    fenotipo_al_exitus = ufmn_parse_phenotype(fenotipo_al_exitus),
    patron_debilidad_inicial = ufmn_parse_distribution(distribucion_al_inicio),
    afectacion_motoneurona_inicial = ufmn_parse_involvement(distribucion_al_inicio),
    predominio_motoneurona_inicial = ufmn_parse_predominance(distribucion_al_inicio),
    estudio_gen_c9 = ufmn_parse_genetic_result(estudio_gen_c9),
    estudio_gen_sod1 = ufmn_parse_genetic_result(estudio_gen_sod1),
    estudio_gen_atxn2 = ufmn_parse_resultado_estudio_atxn2(estudio_gen_otros),
    estudio_gen_kennedy = ufmn_parse_resultado_estudio_kennedy(estudio_gen_otros)
  ) %>%
  select(!c(id, distribucion_al_inicio)) %>%
  relocate(fumador, .after = fenotipo_al_exitus_otro) %>%
  relocate(starts_with("historia_familiar_motoneurona"),
    .after = historia_familiar
  ) %>%
  relocate(starts_with("historia_familiar_alzheimer"),
    .after = historia_familiar_alzheimer
  ) %>%
  relocate(starts_with("historia_familiar_parkinson"),
    .after = historia_familiar_parkinson
  ) %>%
  relocate(historia_familiar_otros,
    .after = historia_familiar_parkinson_grado
  ) %>%
  relocate(patron_debilidad_inicial:predominio_motoneurona_inicial,
    .after = everything()
  ) %>%
  relocate(starts_with("estudio_gen_") & !ends_with("_otros"),
    .after = everything()
  ) %>%
  relocate(estudio_gen_otros, .after = everything()) %>%
  relocate(ends_with("riluzol"), .after = everything()) %>%
  rows_delete(tibble(pid = "9342fe7c-d949-11e9-842a-ebf9c1d8fdac"), by = "pid")

ufmn_nutrition <- DBI::dbReadTable(ufmn_db, "datos_antro") %>%
  rename(
    fecha_visita = fecha_visita_datos_antro,
    imc = imc_actual,
    fecha_inicio_supl_oral = fecha_suplementacion_nutricional,
    fecha_inicio_supl_enteral = fecha_inicio_suplementacion_nutricional_entera,
    estreñimiento = restrenimiento,
    retirada_peg = retirada,
    supl_oral = suplementacion_nutricional_oral,
    supl_enteral = suplementacion_nutricional_entera,
  ) %>%
  rows_update(tibble(id = "40c68842-eeb1-4cd2-a0d8-c5cbc839730c", fecha_visita = NA), by = "id") %>% # was '99-99-9999'
  rows_update(tibble(id = "67e615f4-5f01-11eb-a21b-8316bff80df0", fecha_visita = "03-12-2019"), by = "id") %>% # was 03-12-20219
  rows_update(tibble(id = "f9054526-1dcc-11eb-bb4a-9745fc970131", fecha_indicacion_peg = "23-10-2020"), by = "id") %>% # was 23-10-20020
  rows_update(tibble(id = "8c5b0f46-df7a-11e9-9c30-274ab37b3217", fecha_indicacion_peg = "20-07-3018"), by = "id") %>% # was 20-07-3018
  rows_update(tibble(id = "eb700688-3dfe-11eb-9383-d3a3b2195eff", fecha_complicacion_peg = "22-11-2020"), by = "id") %>% # was 22-11-202
  mutate(across(starts_with("fecha"), \(x) ifelse(x == "29-02-2015", "28-02-2015", x))) %>%
  mutate(across(everything(), ufmn_parse_na),
    across(starts_with("fecha"), ufmn_parse_date),
    across(c(
      estatura, peso, peso_premorbido,
      peso_colocacion_peg, imc
    ), parse_double),
    across(c(
      indicacion_peg, portador_peg, complicacion_peg,
      retirada_peg, espesante, supl_oral, supl_enteral,
      estreñimiento, laxante,
    ), ufmn_parse_logical, true = "Sí", false = "No"),
    across(starts_with("motivo_indicacion_"), ufmn_parse_logical,
      true = "TRUE", false = "FALSE"
    ),
    imc = ifelse(!is.na(imc), imc, round(peso / (estatura / 100)^2, 2)),
    uso_peg = ufmn_parse_peg_usage(uso_peg),
    disfagia = ufmn_parse_dysphagia(disfagia)
  ) %>%
  select(!c(id, created_datetime:updated_datetime)) %>%
  arrange(pid, fecha_visita)

ufmn_respiratory <- DBI::dbReadTable(ufmn_db, "fun_res") %>%
  rename(
    fecha_visita = fecha_visita_fun_res,
    sintomas_hipoventilacion_nocturna = sintomas_sintomas_de_hipoventilacion_nocturna,
    tipo_patologia_respiratoria_intersticial = tipo_patologia_respiratoria_patologia_instersticial,
    sas_apneas_no_claramente_obstructivas = sas_apneas_no_claramanete_obstructivas,
    indicacion_vmni = vmni_indicacion,
  ) %>%
  rows_update(tibble(id = "c2049bdf-4a91-43e0-b6c4-f770881b7499", fecha_visita = NA), by = "id") %>% # was 99-99-9999
  rows_update(tibble(id = "31f94d2a-fb08-11e9-b780-81f732616a71", odi3 = NA), by = "id") %>% # was 17/7
  rows_update(tibble(id = "a3608f72-82eb-11e9-aed7-57f320d0dba4", fecha_realizacion_polisomnografia = NA), by = "id") %>% # was 14
  rows_update(tibble(id = "f508e4b8-db93-11e9-b372-090a91bd3693", fecha_realizacion_polisomnografia = NA), by = "id") %>% # was 14
  mutate(
    across(!c(ends_with("_cual"), cumplimiento_cpap), ufmn_parse_na, na_empty = TRUE),
    across(starts_with("fecha"), ufmn_parse_date),
    across(c(
      starts_with("sintomas_"),
      starts_with("tipo_patologia_respiratoria_")
    ), ufmn_parse_logical, true = "TRUE", false = "FALSE"),
    across(c(
      patologia_respiratoria_previa,
      cpap,
      indicacion_vmni,
      portador_vmni,
      retirada_vmni,
      polisomnografia,
      complicacion_vmni,
    ), ufmn_parse_logical, true = "Sí", false = "No"),
    pcf_por_debajo_del_umbral = pcf == "<60",
    pim_por_debajo_del_umbral = pim == "<60",
    sao2_media_por_debajo_del_umbral = sao2_media == "<90"
  ) %>%
  select(!c(id, created_datetime:updated_datetime)) %>%
  arrange(pid, fecha_visita)

ufmn_functional <- DBI::dbReadTable(ufmn_db, "esc_val_ela") %>%
  rename(
    fecha_visita = fecha_visita_esc_val_ela,
    insuf_resp = insuficiencia_respiratoria,
    kings_r = kings
  ) %>%
  mutate(across(everything(), ufmn_parse_na)) %>%
  filter(!if_all(lenguaje:insuf_resp, is.na)) %>%
  mutate(across(lenguaje:insuf_resp, parse_integer),
    fecha_visita = ufmn_parse_date(fecha_visita)
  ) %>%
  select(!c(id, total:total_bulbar, mitos, created_datetime:updated_datetime)) %>%
  arrange(pid, fecha_visita)

ufmn_followups <- ufmn_functional %>%
  full_join(ufmn_nutrition, by = c("pid", "fecha_visita"), multiple = "all") %>%
  full_join(ufmn_respiratory, by = c("pid", "fecha_visita"), multiple = "all") %>%
  group_by("pid") %>%
  fill() %>%
  ungroup() %>%
  mutate(
    cortar = case_when(
      portador_peg == TRUE ~ cortar_con_peg,
      portador_peg == FALSE ~ cortar_sin_peg,
      cortar_con_peg == cortar_sin_peg ~ cortar_sin_peg,
      is.na(cortar_con_peg) & !is.na(cortar_sin_peg) ~ cortar_sin_peg,
      is.na(cortar_sin_peg) & !is.na(cortar_con_peg) ~ cortar_con_peg,
      cortar_con_peg != 0 & cortar_sin_peg == 0 ~ cortar_con_peg,
      cortar_sin_peg != 0 & cortar_con_peg == 0 ~ cortar_sin_peg,
    ),
    kings_c = case_when(
      disnea == 0 | insuf_resp < 4 ~ "4B",
      indicacion_peg == TRUE ~ "4A",
      indicacion_peg == FALSE ~ {
        bulbar <- any(c(lenguaje, salivacion, deglucion) < 4)
        upper <- any(c(escritura, cortar_sin_peg) < 4)
        lower <- caminar < 4
        as.character(bulbar + upper + lower)
      }
    ),
    mitos = {
      walking_selfcare <- caminar <= 1 | vestido <= 1
      swallowing <- deglucion <= 1
      communication <- lenguaje <= 1 | escritura <= 1
      breathing <- disnea <= 1 | insuf_resp <= 2
      walking_selfcare + swallowing + communication + breathing
    }
  ) %>%
  select(pid, fecha_visita, cortar, kings_c, mitos)

ufmn_functional %<>%
  left_join(ufmn_followups, by = c("pid", "fecha_visita"), multiple = "all") %>%
  mutate(
    cortar_con_peg = ifelse(cortar == cortar_con_peg, cortar_con_peg, NA),
    cortar_sin_peg = ifelse(cortar == cortar_sin_peg, cortar_sin_peg, NA)
  ) %>%
  rowwise() %>%
  mutate(
    alsfrs_bulbar = sum(c_across(lenguaje:deglucion)),
    alsfrs_motor_fino = sum(c_across(escritura:vestido), na.rm = TRUE),
    alsfrs_motor_grosero = sum(c_across(cama:subir_escaleras)),
    alsfrs_respiratorio = sum(c_across(disnea:insuf_resp)),
    alsfrs_total = sum(c_across(lenguaje:insuf_resp), na.rm = TRUE)
  ) %>%
  relocate(cortar, .before = cortar_sin_peg) %>%
  relocate(kings_r:mitos, .after = alsfrs_total)

DBI::dbDisconnect(ufmn_db)

ufmn_export <- function(path, anonimize_data = TRUE) {
  if (!file.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  exports <- list(
    "patients" = ufmn_patients,
    "clinical" = ufmn_clinical,
    "nutrition" = ufmn_nutrition,
    "respiratory" = ufmn_respiratory,
    "functional" = ufmn_functional
  )

  for (key in names(exports)) {
    data <- exports[[key]]
    if (anonimize_data) {
      data <- anonimize(data)
    }
    write_csv(data, file.path(path, paste0("ufmn-", key, ".csv")))
  }
}
