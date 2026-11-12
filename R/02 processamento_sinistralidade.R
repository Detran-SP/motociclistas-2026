library(tidyverse)
library(geobr)
source("R/01 update_clean.R")

# Importação dos dados do Infosiga
sinistros <- clean_infosiga(
  bind_rows(
    read.csv(
      "input/infosiga/sinistros_2022-2024.csv",
      sep = ";"
    ),
    read.csv(
      "input/infosiga/sinistros_2025-2026.csv",
      sep = ";"
    )
  ), file_type = "sinistros") |> 
  filter(data_sinistro > "2024-09-30" & tipo_registro != "Notificação") |> 
  mutate(cod_ibge = as.integer(cod_ibge))

pessoas <- clean_infosiga(
  bind_rows(
    read.csv(
      "input/infosiga/pessoas_2022-2024.csv",
      sep = ";"
    ),
    read.csv(
      "input/infosiga/pessoas_2025-2026.csv",
      sep = ";"
    )
  ), file_type = "pessoas") |> 
  filter(data_sinistro > "2024-09-30") |> 
  left_join(sinistros[,c(1,6)], by = "id_sinistro")

# Importação da base de municípios
bd_municipios <- readxl::read_excel("input/DESEG_Campanha_Municipios.xlsx", skip = 1) |>   
  select(-c("OBITOS_TOTAL", "OBITOS_MOTOCICLISTAS", "SINISTROS_TOTAL", "SINISTROS_MOTOCICLISTAS")) |> 
  left_join(pessoas |> 
              group_by(cod_ibge) |> 
              summarise(OBITOS_TOTAL = sum(gravidade_lesao == "Fatal", na.rm = TRUE),
                        OBITOS_MOTOCICLISTAS = sum(tipo_modo_vitima == "Ocupante de motocicleta", na.rm = TRUE)),
            by = c("CD_MUN_7" = "cod_ibge")) |> 
  left_join(sinistros |> 
              group_by(cod_ibge) |> 
              summarise(SINISTROS_TOTAL = n(),
                        SINISTROS_MOTOCICLISTAS = sum(qtd_motocicleta > 0, na.rm = TRUE)),
            by = c("CD_MUN_7" = "cod_ibge")) |>
  group_by(SUPERINTENDENCIA) |> 
  mutate(PROPORCAO_OBITOS = OBITOS_MOTOCICLISTAS/sum(OBITOS_MOTOCICLISTAS)*100,
         PROPORCAO_SINISTROS = SINISTROS_MOTOCICLISTAS/sum(SINISTROS_MOTOCICLISTAS)*100)

# Atualização da adesão ao SISTRAN com base em data/mun_sistran.csv (se existir)
if (file.exists("data/mun_sistran.csv")) {
  linhas_header <- readLines("data/mun_sistran.csv", n = 2, encoding = "latin1")
  sep_sistran <- if (length(linhas_header) >= 2 && grepl(";", linhas_header[2])) ";" else ","
  tb_sistran <- read.csv("data/mun_sistran.csv", sep = sep_sistran, skip = 1, fileEncoding = "latin1", check.names = FALSE)
  idx_ibge <- grep("ibge", tolower(names(tb_sistran)))[1]
  if (!is.na(idx_ibge)) {
    cods_sistran <- trimws(as.character(tb_sistran[[idx_ibge]]))
    cods_sistran <- cods_sistran[nzchar(cods_sistran)]
    bd_municipios <- bd_municipios |> 
      mutate(ADESAO_SISTRAN = if_else(as.character(CD_MUN_7) %in% cods_sistran, "Sim", "Não"))
  }
}

# Atualização de contatos institucionais adicionais (PRF, DNIT, ANTT, SEMOB, SES-SP)
if (file.exists("data/contatos_drs.csv")) {
  tb_drs <- read.csv("data/contatos_drs.csv", stringsAsFactors = FALSE, check.names = FALSE) |> 
    select(CD_MUN_7 = `Código IBGE`, DRS_REGIAO = Regional, DRS_SETOR = Setor,
           DRS_RESPONSAVEL = `Nome completo`, DRS_CARGO = Cargo,
           DRS_EMAIL = `E-mail`, DRS_TELEFONE = Telefone) |> 
    mutate(CD_MUN_7 = as.integer(CD_MUN_7))
  bd_municipios <- bd_municipios |> 
    left_join(tb_drs, by = "CD_MUN_7") |> 
    mutate(PONTO_FOCAL_DRS = coalesce(DRS_RESPONSAVEL, PONTO_FOCAL_DRS))
}

if (file.exists("data/contatos_prf.csv")) {
  tb_prf <- read.csv("data/contatos_prf.csv", stringsAsFactors = FALSE, check.names = FALSE) |> 
    select(CD_MUN_7 = `Código IBGE`, PRF_DELEGACIA = Delegacia, PRF_RESPONSAVEL = `Nome completo`,
           PRF_CARGO = Cargo, PRF_EMAIL = `E-mail`, PRF_TELEFONE = Telefone) |> 
    mutate(CD_MUN_7 = as.integer(CD_MUN_7))
  bd_municipios <- bd_municipios |> left_join(tb_prf, by = "CD_MUN_7")
}

if (file.exists("data/contatos_dnit.csv")) {
  tb_dnit <- read.csv("data/contatos_dnit.csv", stringsAsFactors = FALSE, check.names = FALSE) |> 
    select(CD_MUN_7 = `Código IBGE`, DNIT_REGIAO = `Região metropolitana`, DNIT_RESPONSAVEL = `Nome completo`,
           DNIT_CARGO = Cargo, DNIT_EMAIL = `E-mail`, DNIT_TELEFONE = Telefone) |> 
    mutate(CD_MUN_7 = as.integer(CD_MUN_7))
  bd_municipios <- bd_municipios |> left_join(tb_dnit, by = "CD_MUN_7")
}

if (file.exists("data/contatos_antt.csv")) {
  tb_antt <- read.csv("data/contatos_antt.csv", stringsAsFactors = FALSE, check.names = FALSE) |> 
    select(CD_MUN_7 = `Código IBGE`, ANTT_RESPONSAVEL = `Nome completo`,
           ANTT_CARGO = Cargo, ANTT_EMAIL = `E-mail`, ANTT_TELEFONE = Telefone) |> 
    mutate(CD_MUN_7 = as.integer(CD_MUN_7))
  bd_municipios <- bd_municipios |> left_join(tb_antt, by = "CD_MUN_7")
}

if (file.exists("data/contatos_guarulhos.csv")) {
  tb_guar <- read.csv("data/contatos_guarulhos.csv", stringsAsFactors = FALSE, check.names = FALSE) |> 
    select(CD_MUN_7 = `Código IBGE`, MUNICIPAL_INSTITUICAO = Instituição, MUNICIPAL_RESPONSAVEL = `Nome completo`,
           MUNICIPAL_CARGO = Cargo, MUNICIPAL_EMAIL = `E-mail`, MUNICIPAL_TELEFONE = Telefone) |> 
    mutate(CD_MUN_7 = as.integer(CD_MUN_7))
  bd_municipios <- bd_municipios |> left_join(tb_guar, by = "CD_MUN_7")
}

# Arquivo geoespacial
sf_municipios <- sf::st_read("input/SP_Municipios.gpkg") |> 
  mutate(CD_MUN_7 = as.integer(CD_MUN)) |> 
  select(CD_MUN_7) |> 
  left_join(bd_municipios, by = "CD_MUN_7")

# Exportação
writexl::write_xlsx(bd_municipios, "output/DESEG_Campanha_Municipios_Dados.xlsx")
sf::st_write(sf_municipios, "output/DESEG_Campanha_Municipios.gpkg", append = FALSE)
