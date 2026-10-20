# Mapa dinâmico dos critérios de alocação

## Visão Geral
Este projeto oferece um fluxo reproduzível para gerar o mapa dinâmico dos critérios de alocação de operações de fiscalização de trânsito. Integra dados de sinistros, condutores envolvidos em sinistros e sistema viário, através da metodologia consolidada no **Caderno Técnico 01 - Análise de Dados para Operações de Fiscalização de Trânsito**.

## Funcionalidades

- Funções modulares que limpam os dados de sinistros do Infosiga.
- Funções modulares que integram os dados de sinistros do Infosiga às células do H3.
- Funções modulares que geram o Índice de Moran Local.
- Integração dos dados de sinistros, condutores envolvidos em sinistros e sistema viário.
- Cálculo do Índice de Moran Local.
- Geração do mapa dinâmico em .html.

## Uso
1. Inputs adicionais: Baixe os arquivos adicionais disponíveis em (link) e insirá-os na pasta `input/`.
2. Definição do período: Altere o valor do objeto data_inicial para definir o período a ser considerado no mapa dinâmico.
3. Execute o script `04 moran.R`.
4. Renderize o script `mapa_dinamica.qmd`.

O mapa dinâmico estará disponível como `mapa_dinamico.html`.

## Estrutura do projeto

```
├── geo/                   # Arquivos adicionais para visualização dos resultados no QGIS
├── input/                 # Bases de dados de entrada
├── output/                # Arquivos intermediários resultantes dos scripts
├── R/                     # Scripts em R
├── .gitignore             # Arquivos ignorados pelo Git
├── fiscalizacao-alocacao  # Projeto do R
├── README.md              # Documentação do projeto
└── LICENSE                # Licença do projeto
```

## Licença

Este projeto está licenciado sob a [Licença GPL-3.0](LICENSE).

## Contato

Divisão de Estudos para Segurança no Trânsito - DETRAN-SP  
📧 [estudos.transito@detran.sp.gov.br](mailto:estudos.transito@detran.sp.gov.br)
