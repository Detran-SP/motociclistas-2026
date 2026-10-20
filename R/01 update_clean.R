library("tidyverse")
source("R/00 sysdata.R")

clean_infosiga <- function(
    df_infosiga,
    file_type = c("sinistros", "pessoas", "veiculos")
) {
    if (file_type == "sinistros") {
        df <- df_infosiga |>
            dplyr::mutate(
                tipo_registro = dplyr::case_match(
                    .data$tipo_registro,
                    "SINISTRO FATAL" ~ "Sinistro fatal",
                    "SINISTRO NAO FATAL" ~ "Sinistro n\u00e3o fatal",
                    "NOTIFICACAO" ~ "Notifica\u00e7\u00e3o",
                ),
                data_sinistro = lubridate::dmy(.data$data_sinistro),
                turno = dplyr::case_match(
                    .data$turno,
                    "MADRUGADA" ~ "Madrugada",
                    "MANHA" ~ "Manh\u00e3",
                    "TARDE" ~ "Tarde",
                    "NOITE" ~ "Noite"
                ),
                numero_logradouro = as.numeric(.data$numero_logradouro),
                tipo_via = dplyr::case_match(
                    .data$tipo_via,
                    "NAO DISPONIVEL" ~ NA,
                    "ESTRADAS E RODOVIAS" ~ "Estradas e rodovias",
                    "VIAS URBANAS" ~ "Vias urbanas"
                ),
                tipo_local = dplyr::case_match(
                    .data$tipo_local,
                    "PUBLICO" ~ "P\u00falico",
                    "PRIVADO" ~ "Privado",
                    "NAO DISPONIVEL" ~ NA
                ),
                regiao_administrativa = dplyr::case_match(
                    .data$regiao_administrativa,
                    "METROPOLITANA DE SÃO PAULO" ~ "Metropolitana de S\u00e3o Paulo",
                    "CAMPINAS" ~ "Campinas",
                    "SOROCABA" ~ "Sorocaba",
                    "SÃO JOSÉ DOS CAMPOS" ~ "S\u00e3o Jos\u00e9 dos Campos",
                    "SÃO JOSÉ DO RIO PRETO" ~ "S\u00e3o Jos\u00e9 do Rio Preto",
                    "BAIXADA SANTISTA" ~ "Baixada Santista",
                    "RIBEIRÃO PRETO" ~ "Ribeir\u00e3o Preto",
                    "CENTRAL" ~ "Central",
                    "BAURU" ~ "Bauru",
                    "MARÍLIA" ~ "Mar\u00edlia",
                    "PRESIDENTE PRUDENTE" ~ "Presidente Prudente",
                    "ARAÇATUBA" ~ "Ara\u00E7atuba",
                    "FRANCA" ~ "Franca",
                    "ITAPEVA" ~ "Itapeva",
                    "REGISTRO" ~ "Registro",
                    "BARRETOS" ~ "Barretos"
                ),
                dplyr::across(
                    c(starts_with("qtd_")),
                    ~ dplyr::if_else(is.na(.x), 0, .x)
                ),
                administracao_via = dplyr::case_match(
                    .data$administracao,
                    c(
                        "CONCESSION\u00c1RIA",
                        "CONCESSION\u00c1RIA-ANTT",
                        "CONCESSION\u00c1RIA-ARTESP"
                    ) ~
                        "Concession\u00e1ria",
                    "NAO DISPONIVEL" ~ NA,
                    "PREFEITURA" ~ "Prefeitura",
                    .default = .data$administracao
                ),
                circunscricao_via = dplyr::case_match(
                    .data$circunscricao,
                    "ESTADUAL" ~ "Estadual",
                    "MUNICIPAL" ~ "Municipal",
                    "FEDERAL" ~ "Federal",
                    "NAO DISPONIVEL" ~ NA
                ),
                tp_sinistro_primario = dplyr::case_match(
                    .data$tp_sinistro_primario,
                    "ATROPELAMENTO" ~ "Atropelamento",
                    "COLISAO" ~ "Colis\u00e3o",
                    "CHOQUE" ~ "Choque",
                    "OUTROS" ~ "Outros",
                    "NAO DISPONIVEL" ~ NA
                ),
                dplyr::across(
                    c(starts_with("tp_sinistro"), -tp_sinistro_primario),
                    ~ dplyr::case_when(
                        .x == "S" ~ 1,
                        is.na(.x) ~ 0
                    )
                ),
            ) |>
            dplyr::left_join(
                y = municipios |> select(cod_ibge, s_ds_municipio),
                by = c("municipio" = "s_ds_municipio", "cod_ibge")
            ) |>
            dplyr::mutate(cod_ibge = as.character(.data$cod_ibge)) |>
            dplyr::left_join(list_ibge_sp, by = "cod_ibge") |>
            dplyr::select(
                .data$id_sinistro,
                .data$data_sinistro,
                .data$hora_sinistro,
                .data$dia_da_semana,
                .data$turno,
                .data$cod_ibge,
                .data$regiao_administrativa,
                .data$nome_municipio,
                .data$logradouro,
                .data$numero_logradouro,
                .data$tipo_via,
                .data$longitude,
                .data$latitude,
                .data$tipo_registro,
                dplyr::starts_with("qtd_"),
                .data$administracao_via,
                .data$conservacao,
                .data$circunscricao_via,
                dplyr::starts_with("tp_sinistro")
            )
    }
    if (file_type == "pessoas") {
        df <- df_infosiga |>
            dplyr::mutate(
                sexo = dplyr::case_match(
                    .data$sexo,
                    "MASCULINO" ~ "Masculino",
                    "FEMININO" ~ "Feminino",
                    "NAO DISPONIVEL" ~ NA
                ),
                data_obito = lubridate::dmy(.data$data_obito),
                tipo_de_vitima = dplyr::case_match(
                    .data$tipo_de_vitima,
                    "CONDUTOR" ~ "Condutor",
                    "PASSAGEIRO" ~ "Passageiro",
                    "PEDESTRE" ~ "Pedestre",
                    "NAO DISPONIVEL" ~ NA
                ),
                tipo_veiculo_vitima = dplyr::case_match(
                    .data$tipo_veiculo_vitima,
                    "MOTOCICLETA" ~ "Motocicleta",
                    "AUTOMOVEL" ~ "Autom\u00f3vel",
                    "NAO DISPONIVEL" ~ NA,
                    "OUTROS" ~ "Outros",
                    "BICICLETA" ~ "Bicicleta",
                    "CAMINHAO" ~ "Caminh\u00e3o",
                    "ONIBUS" ~ "\u00d4nibus",
                    .default = .data$tipo_veiculo_vitima
                ),
                tipo_modo_vitima = dplyr::case_match(
                    .data$tipo_veiculo_vitima,
                    "Motocicleta" ~ "Ocupante de motocicleta",
                    "Autom\u00f3vel" ~ "Ocupante de autom\u00f3vel",
                    "Bicicleta" ~ "Ciclista",
                    "Caminh\u00e3o" ~ "Ocupante de caminh\u00e3o",
                    "\u00d4nibus" ~ "Ocupante de \u00f4nibus",
                    'Outros' ~ "Outros",
                    .default = NA
                ),
                gravidade_lesao = dplyr::case_match(
                    .data$gravidade_lesao,
                    "FATAL" ~ "Fatal",
                    "NAO DISPONIVEL" ~ NA,
                    "LEVE" ~ "Leve",
                    "GRAVE" ~ "Grave"
                ),
                faixa_etaria_demografica = dplyr::case_match(
                    .data$faixa_etaria_demografica,
                    "NAO DISPONIVEL" ~ NA,
                    "90 e +" ~ "90+",
                    .default = .data$faixa_etaria_demografica
                ),
                faixa_etaria_demografica = factor(
                    .data$faixa_etaria_demografica,
                    levels = c(
                        "00 a 04",
                        "05 a 09",
                        "10 a 14",
                        "15 a 19",
                        "20 a 24",
                        "25 a 29",
                        "30 a 34",
                        "35 a 39",
                        "40 a 44",
                        "45 a 49",
                        "50 a 54",
                        "55 a 59",
                        "60 a 64",
                        "65 a 69",
                        "70 a 74",
                        "75 a 79",
                        "80 a 84",
                        "85 a 89",
                        "90+"
                    )
                ),
                faixa_etaria_legal = dplyr::case_match(
                    .data$faixa_etaria_legal,
                    "NAO DISPONIVEL" ~ NA,
                    "80 ou mais" ~ "80+",
                    .default = .data$faixa_etaria_legal
                ),
                faixa_etaria_legal = factor(
                    .data$faixa_etaria_legal,
                    levels = c(
                        "0-17",
                        "18-24",
                        "25-29",
                        "30-34",
                        "35-39",
                        "40-44",
                        "45-49",
                        "50-54",
                        "55-59",
                        "60-64",
                        "65-69",
                        "70-74",
                        "75-79",
                        "80+"
                    )
                ),
                data_sinistro = lubridate::dmy(.data$data_sinistro),
                grau_de_instrucao = dplyr::case_match(
                    .data$grau_de_instrucao,
                    "BÁSICO" ~ "B\u00e1sico",
                    "MÉDIO" ~ "M\u00e9dio",
                    "SUPERIOR" ~ "Superior",
                    .default = .data$grau_de_instrucao
                ),
                local_obito = dplyr::case_match(
                    .data$local_obito,
                    "PUBLICO" ~ "P\u00fablico",
                    "PRIVADO" ~ "Privado",
                    "NAO DISPONIVEL" ~ NA,
                    .default = .data$local_obito
                ),
                tempo_sinistro_obito = dplyr::if_else(is.na(.data$tempo_sinistro_obito), 0, .data$tempo_sinistro_obito)
            ) |>
            dplyr::rename(
                tipo_vitima = .data$tipo_de_vitima
            ) |>
            dplyr::select(
                .data$id_sinistro,
                .data$data_sinistro,
                .data$data_obito,
                .data$sexo,
                .data$idade,
                .data$tipo_vitima,
                .data$faixa_etaria_demografica,
                .data$faixa_etaria_legal,
                .data$tipo_veiculo_vitima,
                .data$tipo_modo_vitima,
                .data$gravidade_lesao,
                .data$profissao,
                .data$grau_de_instrucao,
                .data$local_obito,
                .data$tempo_sinistro_obito
            )
    }

    if (file_type == "veiculos") {
        df <- df_infosiga |>
            dplyr::select(
                .data$id_sinistro,
                ano_fabricacao = .data$ano_fab,
                .data$ano_modelo,
                .data$cor_veiculo,
                .data$tipo_veiculo
            ) |>
            dplyr::mutate(
                tipo_veiculo = dplyr::case_match(
                    .data$tipo_veiculo,
                    "AUTOMOVEL" ~ "Autom\u00f3vel",
                    "MOTOCICLETA" ~ "Motocicleta",
                    "CAMINHAO" ~ "Caminh\u00e3o",
                    "ONIBUS" ~ "\u00d4nibus",
                    "OUTROS" ~ "Outros",
                    "BICICLETA" ~ "Bicicleta",
                    "NAO DISPONIVEL" ~ NA
                )
            )
    }
    return(df)
}