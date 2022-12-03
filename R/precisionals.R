library(dplyr)
library(readr)

source("R/fmv.R")
source("R/ufmn.R")
source("R/imegen.R")
source("R/metrosud.R")
source("R/sectecnica.R")

precisionals_recode_sex <- function(data) {
  data %>% recode_factor(
    Hombre = "Male",
    Mujer = "Female"
  )
}

precisionals_recode_studies <- function(data) {
  data %>% recode_factor(
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
  data %>% recode_factor(
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
  data %>% recode_factor(
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
  data %>% recode_factor(
    Fumador = "Active",
    Exfumador = "Ceased",
    `No fumador` = "Never"
  )
}

precisionals_recode_cognitive_status <- function(data) {
  data %>% recode_factor(
    DTA = "AD",
    DFT = "FTD",
    `DCL-Cognitivo` = "MCI",
    `DCL-Conductual` = "MBI",
    `DCL-Mixto` = "MBI+MCI",
    Otros = "Other"
  )
}

precisionals_recode_weakness_pattern <- function(data) {
  data %>% recode_factor(
    Respiratoria = "Respiratory",
    MMSS = "Upper limbs",
    MMII = "Lower limbs",
    `MMSS+MMII` = "All limbs"
  )
}

precisionals_recode_mn_involvement <- function(data) {
  data %>% recode_factor(
    Ninguno = "None",
    MNS = "UMN",
    MNI = "LMN",
    `MNS+MNI` = "UMN+LMN"
  )
}

precisionals_recode_discharge_type <- function(data) {
  data %>% recode_factor(
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
  data %>% recode_factor(Alterado = "Altered")
}

precisionals_recode_dysphagia <- function(data) {
  data %>% recode_factor(
    No = "None",
    Liquidos = "Liquids",
    Solidos = "Solids",
    Mixta = "Mixed"
  )
}

precisionals_recode_peg_usage <- function(data) {
  data %>% recode_factor(
    Hidratacion = "Hydration",
    `Hidratacion + Medicacion` = "Hydration + Medication",
    `Hidratacion + Medicacion + Nutricion parcial` =
      "Hydration + Medication + Parcial nutrition",
    `Nutricion completa` = "Complete nutrition"
  )
}

precisionals_recode_gpvars_names <- function(data) {
  data %>% recode_factor(
    ABVDB = "Barthel",
    SITEN06 = "Barthel",
    SITGE05 = "Barthel",
    AIVDL = "Lawton-Brody",
    VMTPF = "Pfeiffer",
    SITGE06 = "Pfeiffer",
    TT102 = "Weight",
    TT103 = "BMI",
    .default = NA_character_
  )
}

precisionals_recode_income_source <- function(data) {
  data %>% recode_factor(
    `No ingresos` = "None",
    `En paro` = "Unemployment aid",
    Trabaja = "Work",
    `Baja laboral` = "Sick leave",
    Jubilacion = "Retirement",
    Incapacidad = "Disability aid",
    `Pension no contributiva` = "Disability aid",
    `Pension de viudedad` = "Widowhood aid",
    `Seguro obligatorio vejez e invalidez` = "Other government aids"
  )
}

precisionals_recode_application_status <- function(data) {
  data %>% recode_factor(
    Tramitada = TRUE,
    `Pendiente` = FALSE,
    `No tramitada` = FALSE
  )
}

precisionals_recode_disability_level <- function(data) {
  data %>% recode_factor(
    IT = "Temporary",
    IPP = "Partial",
    IPT = "Total",
    IPA = "Absolute",
    GI = "Absolute"
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
  select(
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
  mutate(
    discharge_type = precisionals_recode_discharge_type(destino_al_alta),
  ) %>%
  select(
    hospital_id = nhc,
    episode_id = episodio,
    admission_date = fecha_ingreso,
    dx_encoding = codif_diagnostico,
    dx_code = cod_diagnostico,
    dx_description = desc_diagnostico,
    discharge_date = fecha_alta,
    discharge_type,
    discharge_dept = servicio_alta,
    destination_centre = centro_destino_al_alta
  )

precisionals_nutrition <- ufmn_nutrition %>%
  mutate(
    dysphagia = precisionals_recode_dysphagia(disfagia),
    peg_usage = precisionals_recode_peg_usage(uso_peg)
  ) %>%
  select(
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
    peg_usage,
    peg_complication = complicacion_peg,
    peg_complication_date = fecha_complicacion_peg,
    peg_removal = retirada_peg,
    peg_removal_date = fecha_retirada_peg,
    dysphagia,
    food_thickener = espesante,
    food_thickener_start = fecha_inicio_espesante,
    oral_suppl = supl_oral,
    oral_suppl_start = fecha_inicio_supl_oral,
    enteral_suppl = supl_enteral,
    enteral_suppl_start = fecha_inicio_supl_enteral,
    constipation = estreñimiento,
    laxative_usage = laxante,
  )

precisionals_respiratory <- ufmn_respiratory %>%
  select(
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
    psg_ct90 = ct90_polisomnografia,
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

precisionals_genesets <- imegen_paneles %>%
  rename(
    geneset = panel,
    gene = gen,
    transcript = transcrito
  )

precisionals_genetics_ext <- imegen_resultados %>%
  left_join(ufmn_patients, by = "nhc") %>%
  mutate(
    genotype = recode(genotipo, heterocigosis = "heterozygous"),
    interpretation = recode(interpretacion,
      "Probablemente Patogénica" = "Likely pathogenic",
      "Significado clinico incierto" = "Uncertain significance",
      "Factor de riesgo / susceptibilidad" = "Risk factor / susceptibility"
    )
  ) %>%
  select(
    hospital_id = nhc,
    geneset = panel,
    gene = gen,
    allele1 = alelo1,
    allele2 = alelo2,
    HGVSc,
    HGVSp,
    genotype,
    inheritance = herencia,
    interpretation
  )

precisionals_comorbidities <- ufmn_patients %>%
  mutate(cip_parcial = substr(cip, 1, 13)) %>%
  inner_join(metrosud_problemas, by = "cip_parcial") %>%
  select(
    patient_id = pid,
    dx_date = fecha_problema,
    dx_code = cod_problema,
    dx_encoding = codif_problema,
    dx_description = desc_problema
  )

precisionals_gpvisits <- ufmn_patients %>%
  mutate(cip_parcial = substr(cip, 1, 13)) %>%
  inner_join(metrosud_visitas, by = "cip_parcial") %>%
  select(
    patient_id = pid,
    date = fecha_visita,
    dx_code = cod_problema,
    dx_encoding = codif_problema,
    dx_description = desc_problema
  )

precisionals_treatments <- ufmn_patients %>%
  mutate(cip_parcial = substr(cip, 1, 13)) %>%
  inner_join(metrosud_farmacia, by = "cip_parcial") %>%
  select(
    patient_id = pid,
    prescription_start = fecha_inicio,
    prescription_end = fecha_fin,
    product_code = cod_producto,
    product_description = desc_producto,
    actcompound_code = cod_principio,
    actcompound_description = desc_principio
  )

precisionals_social <- ufmn_patients %>%
  inner_join(fmv_data, by = c("dni" = "nif")) %>%
  mutate(
    across(c(starts_with("estado_")), precisionals_recode_application_status),
    situacion_laboral = precisionals_recode_income_source(situacion_laboral),
    tipo_incapacidad = precisionals_recode_disability_level(tipo_incapacidad),
  ) %>%
  select(
    patient_id = pid,
    income_source = situacion_laboral,
    has_family_caregiver = cuidador_familiar,
    has_profesional_caregiver = cuidador_profesional,
    disability_requested = estado_incapacidad,
    disability_request_date = fecha_tramite_incapacidad,
    disability_level = tipo_incapacidad,
    dependency_requested = estado_lapad,
    dependency_request_date = fecha_tramite_lapad,
    dependency_degree = grado_lapad,
  )

precisionals_gpvars <- ufmn_patients %>%
  mutate(cip_parcial = substr(cip, 1, 13)) %>%
  inner_join(metrosud_variables, by = "cip_parcial") %>%
  mutate(
    var_name = precisionals_recode_gpvars_names(cod_variable),
  ) %>%
  select(
    patient_id = pid,
    date = fecha_registro,
    var_name, value = valor
  ) %>%
  drop_na(patient_id, var_name)

precisionals_export <- function(path, anonimize_data = TRUE) {
  if (!file.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  exports <- list(
    "alsfrs_r" = precisionals_alsfrs,
    "comorbidities" = precisionals_comorbidities,
    "er_diagnoses" = precisionals_er_diagnoses,
    "er_episodes" = precisionals_er_episodes,
    "genesets" = precisionals_genesets,
    "genetics" = precisionals_genetics,
    "genetics_ext" = precisionals_genetics_ext,
    "hospitalizations" = precisionals_hosp,
    "gpvisits" = precisionals_gpvisits,
    "gpvars" = precisionals_gpvars,
    "nutrition" = precisionals_nutrition,
    "patients" = precisionals_patients,
    "respiratory" = precisionals_respiratory,
    "social" = precisionals_social,
    "treatments" = precisionals_treatments
  )

  for (key in names(exports)) {
    data <- exports[[key]]
    if (anonimize_data) {
      data <- anonimize(data)
    }
    write_csv(data, file.path(path, paste0("precisionals-", key, ".csv")))
  }
}
