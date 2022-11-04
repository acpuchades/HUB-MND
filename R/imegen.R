
imegen_data_path <- "data/imegen-2022_11_04.xlsx"

imegen_results <-
    readxl::read_excel(imegen_data_path) %>%
    setNames(c(
        "imegen_id", "nhc", "panel", "gen", "alelo1", "alelo2", "HGVSc", "HGVSp",
        "dbSNP", "maf", "genotipo", "herencia", "interpretacion"
    )) %>%
    dplyr::mutate(
        across(c(panel, genotipo, interpretacion, herencia), factor),
    )
