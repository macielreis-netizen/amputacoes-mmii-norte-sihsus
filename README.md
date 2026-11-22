# amputacoes-mmii-norte-sihsus
Códigos em R: amputações não traumáticas de membros inferiores, Pará e Região Norte, SIH/SUS 2015–2025
Códigos em R do estudo "Tendência temporal, custos e mortalidade hospitalar por amputações não traumáticas de membros inferiores no Pará e na Região Norte do Brasil, 2015–2025: estudo ecológico de séries temporais".

## Ordem de execução

1. `01_extracao.R`: download dos arquivos do SIH/SUS (SIH-RD) dos sete estados da Região Norte, 2015–2025, pelo pacote microdatasus.
2. `02_preparo.R`: seleção das internações (amputações e revascularizações) e preparo das variáveis.
3. `03_analise.R`: taxas padronizadas por idade, tendência (Prais-Winsten), custos, mortalidade hospitalar e razão amputação/revascularização.

## Dados

Os dados brutos não estão neste repositório. São públicos e foram obtidos do DATASUS (SIH/SUS) e do IBGE (projeções da população, Revisão 2024). O script `01_extracao.R` baixa novamente os arquivos.

## Software

R, com os pacotes dplyr, epitools, broom, MASS e microdatasus, entre outros.

## Citação

Reis JMC. Código do estudo sobre amputações não traumáticas de membros inferiores no Pará e na Região Norte, SIH/SUS, 2015–2025. Zenodo; 2026.
