
imegen_data_path <- "data/imegen-2022_11_04.xlsx"

imegen_results <-
    readxl::read_excel(imegen_data_path) %>%
    rename(
        imegen_id = ID,
        nhc = NHC,
        fecha_estudio = `Fecha del estudio`,
        panel = `Panel estudiado`,
        gen = `Gen alterado`,
        alelo1 = `Alelo 1`,
        alelo2 = `Alelo 2`,
        maf = MAF,
        genotipo = Genotipo,
        herencia = Herencia,
        interpretacion = `Interpretación`
    ) %>%
    dplyr::mutate(
        across(c(panel, genotipo, interpretacion, herencia), factor),
    )
