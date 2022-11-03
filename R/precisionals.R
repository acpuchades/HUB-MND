library(dplyr)
library(readr)

source("R/anonimize.R")
source("R/ufmn.R")
source("R/sectecnica.R")

precisionals_recode_sex <- function(data) {
  data %>% recode(
    Hombre = "Male",
    Mujer = "Female"
  )
}

precisionals_recode_studies <- function(data) {
  data %>% recode(
    Otros = "Other",
    Analfabeto = "Illiterate",
    `Primarios incompletos` = "Primary education (incomplete)",
    `Primarios completos` = "Primary education (complete)",
    Secundarios = "Secondary education",
    `Formacion profesional` = "Higher education",
    Universitarios = "University degree",
    Doctorado = "Doctorate"
  )
}

precisionals_recode_working_status <- function(data) {
  data %>% recode(
    Otros = "Other",
    Trabaja = "Working",
    `Labores de la casa` = "Working at home",
    `En paro (con subsidio)` = "Unemployed (with subsidy)",
    `En paro (sin subsidio)` = "Unemployed (without subsidy)",
    Incapacitado = "Disabled",
    Jubilado = "Retired"
  )
}

precisionals_recode_phenotype <- function(data) {
  data %>% recode(
    AMP = "PMA",
    ELP = "PLS",
    `ELA Bulbar` = "Bulbar",
    `ELA Espinal` = "Spinal",
    `ELA Respiratoria` = "Respiratory",
    Hemiplejica = "Hemiplegic",
    Monomielica = "Monomyelic",
    Pseudopolineuritica = "Pseudopolyneuritic",
    Otro = "Other"
  )
}

precisionals_recode_smoking_status <- function(data) {
  data %>% recode(
    Fumador = "Active",
    Exfumador = "Ceased",
    `No fumador` = "Never"
  )
}

precisionals_recode_cognitive_status <- function(data) {
  data %>% recode(
    DTA = "AD",
    DFT = "FTD",
    `DCL-Cognitivo` = "MCI",
    `DCL-Conductual` = "MBI",
    `DCL-Mixto` = "MBI+MCI",
    Otros = "Other"
  )
}

precisionals_recode_weakness_pattern <- function(data) {
  data %>% recode(
    Respiratoria = "Respiratory",
    MMSS = "Upper limbs",
    MMII = "Lower limbs",
    `MMSS+MMII` = "All limbs"
  )
}

precisionals_recode_mn_involvement <- function(data) {
  data %>% recode(
    Ninguno = "None",
    MNS = "UMN",
    MNI = "LMN",
    `MNS+MNI` = "UMN+LMN"
  )
}

precisionals_recode_discharge_type <- function(data) {
  data %>% recode(
    `Alta voluntaria` = "DAMA",
    Domicilio = "Planned",
    Fuga = "Abscond",
    Ingreso = "Admitted",
    `No atendido` = "Not attended",
    Traslado = "Transferred",
    Sociosanitario = "Rehabilitation",
    `Hosp. domiciliaria` = "Home hospitalization",
    Exitus = "Death"
  )
}

precisionals_recode_genetic_result <- function(data) {
  data %>% recode(Alterado = "Altered")
}

precisionals_recode_dysphagia <- function(data) {
  data %>% recode(
    No = "None",
    Liquidos = "Liquids",
    Solidos = "Solids",
    Mixta = "Mixed"
  )
}

precisionals_recode_peg_usage <- function(data) {
  data %>% recode(
    Hidratacion = "Hydration",
    `Hidratacion + Medicacion` = "Hydration + Medication",
    `Hidratacion + Medicacion + Nutricion parcial` =
      "Hydration + Medication + Parcial nutrition",
    `Nutricion completa` = "Complete nutrition"
  )
}

precisionals_patients <- ufmn_patients %>%
  left_join(ufmn_clinical, by = "pid") %>%
  select(!c(
    provincia_nacimiento,
    provincia_residencia,
    municipio_residencia,
    abs,
    hospital_referencia,
    trabajo_actual,
    tiempo_trabajado,
    ultima_ocupacion_al_inicio,
    trabajo_interes,
    tiempo_inactividad,
    situacion_laboral_al_inicio_otro,
    estudios_otros,
    historia_familiar_otros,
    antecedentes_otros,
    starts_with("estudio_gen_"),
    ends_with("_grado"),
    fenotipo_al_diagnostico_otro,
    fenotipo_al_exitus_otro,
    resultado_estudio_cognitivo_otros
  )) %>%
  rename(
    patient_id = pid,
    hospital_id = nhc,
    ihc_id = cip,
    birthdate = fecha_nacimiento,
    zip_code = codigo_postal,
    first_visit = fecha_primera_visita,
    fh_als = historia_familiar_motoneurona,
    fh_alzheimer = historia_familiar_alzheimer,
    fh_parkinson = historia_familiar_parkinson,
    cognitive_imp = deterioro_cognitivo,
    dx_date = fecha_diagnostico,
    clinical_onset = fecha_inicio_clinica,
    riluzole_received = riluzol,
    riluzole_start = fecha_inicio_riluzol,
    death = exitus,
    death_date = fecha_exitus,
  ) %>%
  mutate(
    site = "Bellvitge Hospital (Barcelona)",
    sex = precisionals_recode_sex(sexo),
    studies = precisionals_recode_studies(estudios),
    working_status = precisionals_recode_working_status(situacion_laboral_al_inicio),
    smoking = precisionals_recode_smoking_status(fumador),
    last_followup = ufmn_followups %>%
      left_join(ufmn_patients, ., by = "pid") %>%
      group_by(pid) %>%
      arrange(fecha_visita, .by_group = TRUE) %>%
      summarize(ultimo_seguimiento = last(fecha_visita)) %$%
      ultimo_seguimiento,
    phenotype_dx = precisionals_recode_phenotype(fenotipo_al_diagnostico),
    phenotype_death = precisionals_recode_phenotype(fenotipo_al_exitus),
    cognitive_dx = precisionals_recode_cognitive_status(resultado_estudio_cognitivo),
    mn_involvement = precisionals_recode_mn_involvement(afectacion_motoneurona_inicial),
    mn_predominance = precisionals_recode_mn_involvement(predominio_motoneurona_inicial),
    weakness_pattern = precisionals_recode_weakness_pattern(patron_debilidad_inicial),
    imv_support = ufmn_functional %>%
      left_join(ufmn_patients, ., by = "pid") %>%
      group_by(pid) %>%
      arrange(fecha_visita, .by_group = TRUE) %>%
      summarize(fecha_inicio = first(fecha_visita)) %$%
      fecha_inicio,
    .keep = "unused",
  ) %>%
  relocate(site, .before = everything())

precisionals_alsfrs <- ufmn_functional %>%
  rename(
    patient_id = pid,
    assessment_date = fecha_visita,
    speech = lenguaje,
    salivation = salivacion,
    swallowing = deglucion,
    handwriting = escritura,
    cutting = cortar,
    cutting_peg = cortar_con_peg,
    cutting_no_peg = cortar_sin_peg,
    dressing = vestido,
    bed = cama,
    walking = caminar,
    stairs = subir_escaleras,
    dyspnea = disnea,
    orthopnea = ortopnea,
    resp_insuf = insuf_resp,
    alsfrs_fine_motor = alsfrs_motor_fino,
    alsfrs_gross_motor = alsfrs_motor_grosero,
    alsfrs_respiratory = alsfrs_respiratorio
  )

precisionals_er_episodes <- sectecnica_urg_episodios %>%
  rename(
    hospital_id = nhc,
    episode_id = episodio,
    admission_date = fecha_entrada,
    discharge_date = fecha_salida,
    destination_centre = centro_destino_al_alta
  ) %>%
  mutate(
    discharge_type = precisionals_recode_discharge_type(destino_al_alta),
    .keep = "unused",
  ) %>%
  relocate(hospital_id, .before = everything()) %>%
  relocate(discharge_type, .after = discharge_date)

precisionals_er_diagnoses <- sectecnica_urg_diagnosticos %>%
  rename(
    episode_id = episodio,
    dx_code = cod_diagnostico,
    dx_description = desc_diagnostico,
    dx_encoding = codif_diagnostico
  ) %>%
  drop_na(dx_code)

precisionals_genetics <- ufmn_clinical %>%
  select(c(pid, starts_with("estudio_gen_"))) %>%
  rename(
    patient_id = pid,
    c9_status = estudio_gen_c9,
    sod1_status = estudio_gen_sod1,
    atxn2_status = estudio_gen_atxn2,
    kennedy_status = estudio_gen_kennedy,
    other_genes = estudio_gen_otros
  ) %>%
  mutate(
    across(ends_with("_status"), precisionals_recode_genetic_result),
  )

precisionals_hosp <- sectecnica_hosp %>%
  rename(
    hospital_id = nhc,
    episode_id = episodio,
    admission_date = fecha_ingreso,
    dx_encoding = codif_diagnostico,
    dx_code = cod_diagnostico,
    dx_description = desc_diagnostico,
    discharge_date = fecha_alta,
    discharge_dept = servicio_alta,
    destination_centre = centro_destino_al_alta
  ) %>%
  mutate(
    discharge_type = precisionals_recode_discharge_type(destino_al_alta),
    .keep = "unused",
  ) %>%
  relocate(hospital_id, .before = everything()) %>%
  relocate(starts_with("discharge_"), .after = starts_with("dx_"))

precisionals_nutrition <- ufmn_nutrition %>%
  select(!c(
    laxante_cual,
    espesante_cual,
    aporte_kcal_extra,
    aporte_kcal_extra_tomado,
    aporte_kcal_extra_tomado_entero,
    aporte_kcal_extra_prescrito_entero,
    test_eat10,
    modificacion_textura,
    modificacion_textura_otra,
    alsspuntuacion,
    motivo_retirada_peg,
    ends_with("_cual"),
    ends_with("_otro"),
    ends_with("_otros")
  )) %>%
  rename(
    patient_id = pid,
    assessment_date = fecha_visita,
    weight = peso,
    weigh_date = fecha_peso,
    height = estatura,
    bmi = imc,
    premorbid_weight = peso_premorbido,
    premorbid_weight_date = fecha_peso_premorbido,
    peg_indication = indicacion_peg,
    peg_indication_date = fecha_indicacion_peg,
    peg_indication_reason_dysphagia = motivo_indicacion_peg_disfagia,
    peg_indication_reason_weightloss = motivo_indicacion_peg_perdida_de_peso,
    peg_indication_reason_respinsuf = motivo_indicacion_peg_insuficiencia_respiratoria,
    peg_carrier = portador_peg,
    peg_placement_date = fecha_colocacion_peg,
    peg_placement_weight = peso_colocacion_peg,
    peg_complication = complicacion_peg,
    peg_complication_date = fecha_complicacion_peg,
    peg_removal = retirada_peg,
    peg_removal_date = fecha_retirada_peg,
    food_thickener = espesante,
    food_thickener_start = fecha_inicio_espesante,
    oral_suppl = supl_oral,
    oral_suppl_start = fecha_inicio_supl_oral,
    enteral_suppl = supl_enteral,
    enteral_suppl_start = fecha_inicio_supl_enteral,
    constipation = estreñimiento,
    laxative_usage = laxante,
  ) %>%
  mutate(
    dysphagia = precisionals_recode_dysphagia(disfagia),
    peg_usage = precisionals_recode_peg_usage(uso_peg),
    .keep = "unused",
  )

precisionals_respiratory <- ufmn_respiratory %>%
  select(!c(
    patologia_respiratoria_previa,
    tipo_patologia_respiratoria_nsnc,
    cumplimiento_cpap,
    motivo_colocacion_vmni,
    ends_with("_cual"),
    sas_no,
  )) %>%
  rename(
    patient_id = pid,
    assessment_date = fecha_visita,
    copd_history = tipo_patologia_respiratoria_epoc,
    asthma_history = tipo_patologia_respiratoria_asma,
    bronchiectasis_history = tipo_patologia_respiratoria_bronquiectasias,
    interstitial_history = tipo_patologia_respiratoria_intersticial,
    sahs_history = tipo_patologia_respiratoria_saos,
    other_respiratory_history = tipo_patologia_respiratoria_otra,
    pcf_below_threshold = pcf_por_debajo_del_umbral,
    fvc_sitting = fvc_sentado,
    fvc_sitting_abs = fvc_sentado_absoluto,
    fvc_lying = fvc_estirado,
    fvc_lying_abs = fvc_estirado_absoluto,
    mip = pim,
    mip_below_threshold = pim_por_debajo_del_umbral,
    mep = pem,
    abg_ph = ph_sangre_arterial,
    abg_po2 = pao2,
    abg_pco2 = paco2,
    abg_hco3 = hco3,
    npo_mean_spo2 = sao2_media,
    npo_mean_spo2_below_threshold = sao2_media_por_debajo_del_umbral,
    npo_ct90 = ct90,
    npo_odi3 = odi3,
    polisomnography_performed = polisomnografia,
    polisomnography_date = fecha_realizacion_polisomnografia,
    psg_ct90 = ct90,
    psg_ahi = iah,
    psg_apneas_obstr = sas_apneas_obstructivas,
    psg_apneas_nonobstr = sas_apneas_no_claramente_obstructivas,
    psg_apneas_central = sas_apneas_centrales,
    psg_apneas_mixed = sas_apneas_mixtas,
    orthopnea = sintomas_intolerancia_al_decubito,
    exertive_dyspnea = sintomas_disnea_de_esfuerzo,
    night_hypoventilation_symptoms = sintomas_hipoventilacion_nocturna,
    ineffective_cough = sintomas_tos_ineficaz,
    cpap = cpap,
    cpap_date = fecha_cpap,
    niv_indication = indicacion_vmni,
    niv_indication_date = fecha_indicacion_vmni,
    niv_indication_reason_symptoms = motivo_indicacion_vmni_sintomas,
    niv_indication_reason_fvc = motivo_indicacion_vmni_fvc,
    niv_indication_reason_sleepdesat = motivo_indicacion_vmni_desaturacion_nocturna,
    niv_indication_reason_sleephypercap = motivo_indicacion_vmni_hipercapnia_nocturna,
    niv_indication_reason_awakehypercap = motivo_indicacion_vmni_hipercapnia_diurna,
    niv_indication_reason_other = motivo_indicacion_vmni_otros,
    niv_placement = portador_vmni,
    niv_placement_date = fecha_colocacion_vmni,
    niv_complication = complicacion_vmni,
    niv_complication_date = fecha_complicacion_vmni,
    niv_complication_nasalulc = motivo_complicacion_vmni_ulcera_nasal_por_presion,
    niv_complication_aerophagia = motivo_complicacion_vmni_aerofagia,
    niv_complication_dryness = motivo_complicacion_vmni_sequedad_orofaringea,
    niv_complication_other = motivo_complicacion_vmni_otros,
    niv_stopped = retirada_vmni,
    niv_stopped_date = fecha_retirada_vmni,
    niv_stopped_reason_intolerance = motivo_retirada_vmi_intolerancia,
    niv_stopped_reason_compliance = motivo_retirada_vmi_no_cumplimiento,
    niv_stopped_reason_voluntary = motivo_retirada_vmi_rechazo_del_paciente,
    niv_stopped_reason_other = motivo_retirada_vmi_otros
  )

precisionals_export <- function(path, anonimize_data = TRUE) {
  if (!file.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  exports <- list(
    "patients" = precisionals_patients,
    "alsfrs_r" = precisionals_alsfrs,
    "nutrition" = precisionals_nutrition,
    "respiratory" = precisionals_respiratory,
    "genetics" = precisionals_genetics,
    "er_episodes" = precisionals_er_episodes,
    "er_diagnoses" = precisionals_er_diagnoses,
    "hospitalizations" = precisionals_hosp
  )

  for (key in names(exports)) {
    data <- exports[[key]]
    if (anonimize_data) {
      data <- anonimize(data)
    }
    write_csv(data, file.path(path, paste0("precisionals-", key, ".csv")))
  }
}
