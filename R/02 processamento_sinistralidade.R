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

# Arquivo geoespacial
sf_municipios <- sf::st_read("input/SP_Municipios.gpkg") |> 
  mutate(CD_MUN_7 = as.integer(CD_MUN)) |> 
  select(CD_MUN_7) |> 
  left_join(bd_municipios, by = "CD_MUN_7")

# Exportação
writexl::write_xlsx(bd_municipios, "output/DESEG_Campanha_Municipios_Dados.xlsx")
sf::st_write(sf_municipios, "output/DESEG_Campanha_Municipios.gpkg", append = FALSE)
