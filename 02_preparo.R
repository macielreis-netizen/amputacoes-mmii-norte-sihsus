# ==============================================================================
# 02_preparo.R
# Limpeza, classificação, correção monetária (IPCA), populações e taxas
# padronizadas por idade.
#
# ENTRADAS (na pasta do projeto)
#   resultados/sih_filtrado_bruto.rds             (gerado por 01_extracao.R)
#   projecoes_2024_tab2_grupo_quinquenal.xlsx     (IBGE, Projeções Revisão 2024)
# SAÍDAS (em resultados/)
#   base_analitica.rds, taxas_padronizadas.csv, fluxo_amputacoes.csv,
#   fluxo_revascularizacoes.csv, qc_*.csv, ipca_sidra_1737.csv
#
# COMO USAR: Session > Set Working Directory > To Source File Location; Source.
# ==============================================================================

pacotes <- c("dplyr", "tidyr", "stringr", "readr", "readxl", "sidrar", "epitools", "tibble")
faltando <- setdiff(pacotes, rownames(installed.packages()))
if (length(faltando)) install.packages(faltando)
invisible(lapply(pacotes, library, character.only = TRUE))

PASTA <- getwd()
arq_base <- file.path(PASTA, "resultados", "sih_filtrado_bruto.rds")
arq_pop  <- file.path(PASTA, "projecoes_2024_tab2_grupo_quinquenal.xlsx")
stopifnot("Rode 01_extracao.R antes" = file.exists(arq_base),
          "xlsx de populacao nao encontrado na pasta" = file.exists(arq_pop))

# ---- Parâmetros (devem ser idênticos aos de 01_extracao.R) -------------------
UFS <- c("AC", "AM", "AP", "PA", "RO", "RR", "TO")
COD_AMP_MAIOR  <- "0408050012"
COD_AMP_PE     <- "0408050020"
COD_REV_ABERTA <- c("0406020329", "0406020345", "0406020353", "0406020361",
                    "0406020388", "0406020434", "0406020442", "0406020450")
COD_REV_ENDO   <- c("0406040028", "0406040044", "0406040052",
                    "0406040060", "0406040079", "0406040281")
COD_REV_SENS   <- c("0406020124", "0406020302", "0406020396")
ANO_BASE_IPCA  <- 202512     # preços de dezembro de 2025
ANO_PADRAO     <- 2022       # população-padrão: Brasil, 2022, ambos os sexos
CID4_REVASC <- c("I731", "I738", "I739", "I740", "I741", "I743",
                 "I744", "I745", "I748", "I749", "I771", "I776")

num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x))))
faixas_lv <- c("00-04", "05-09", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
               "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")

# ==============================================================================
# 1. Conversão de tipos e variáveis derivadas
# ==============================================================================
base <- readRDS(arq_base)
message("Registros lidos: ", nrow(base))

base <- base %>%
  mutate(
    ano  = as.integer(ANO_CMPT),
    mes  = as.integer(MES_CMPT),
    idade_cod = as.character(COD_IDADE),
    idade_num = num(IDADE),
    idade_anos = case_when(
      str_detect(idade_cod, regex("^Anos", ignore_case = TRUE)) | idade_cod == "4" ~ idade_num,
      str_detect(idade_cod, regex("^Meses|^Dias", ignore_case = TRUE)) | idade_cod %in% c("2", "3") ~ 0,
      str_detect(idade_cod, regex("Centena", ignore_case = TRUE)) | idade_cod == "5" ~ 100 + idade_num,
      TRUE ~ NA_real_),
    faixa5 = cut(idade_anos, breaks = c(seq(0, 80, 5), Inf), right = FALSE, labels = faixas_lv),
    sexo = case_when(SEXO %in% c("Masculino", "1") ~ "Homens",
                     SEXO %in% c("Feminino", "3")  ~ "Mulheres",
                     TRUE ~ NA_character_),
    obito = MORTE %in% c("Sim", "1"),
    val_tot  = num(VAL_TOT),
    dias_perm = num(DIAS_PERM),
    car_int3 = case_when(CAR_INT == "01" ~ "Eletivo (01)",
                         CAR_INT == "02" ~ "Urgência (02)",
                         TRUE ~ "Demais códigos (05/06)"),
    dt_inter = dplyr::coalesce(as.Date(DT_INTER, format = "%Y-%m-%d"), as.Date(DT_INTER, format = "%Y%m%d")),
    dt_saida = as.Date(DT_SAIDA, format = "%Y%m%d"),
    proc = case_when(PROC_REA == COD_AMP_MAIOR ~ "amp_maior",
                     PROC_REA == COD_AMP_PE ~ "amp_pe",
                     PROC_REA %in% COD_REV_ABERTA ~ "rev_aberta",
                     PROC_REA %in% COD_REV_ENDO ~ "rev_endo",
                     PROC_REA %in% COD_REV_SENS ~ "rev_sens"),
    tipo = if_else(proc %in% c("amp_maior", "amp_pe"), "Amputação", "Revascularização"),
    cid3 = str_sub(toupper(str_trim(DIAG_PRINC)), 1, 3),
    cid4 = str_sub(toupper(str_trim(DIAG_PRINC)), 1, 4),
    grupo_cid = case_when(
      str_starts(cid3, "S") ~ "Traumática (S)",
      str_starts(cid3, "Q") ~ "Congênita (Q)",
      str_starts(cid3, "Z") ~ "Excluída: CID Z (ex.: Z89 ausência adquirida de membro)",
      cid3 == "T87"         ~ "T87 (complicação de coto)",
      str_starts(cid3, "T") ~ "Outras do capítulo T",
      TRUE                  ~ "Não traumática"),
    # Grupo diagnóstico operacional (definição do autor). R02 e A48 (gangrena) são agrupados à parte: o CID principal
    # não identifica a doença de base e a codificação varia entre UFs e no tempo (ver qc_top_cid_amputacao.csv).
    dx = case_when(
      cid3 %in% paste0("E", 10:14) ~ "Diabetes (E10-E14)",
      str_detect(cid3, "^I(7[0-9]|96)$") ~ "Doença vascular (I70-I79, I96)",
      cid3 %in% c("R02", "A48") ~ "Gangrena/infecção necrosante inespecífica (R02, A48)",
      str_detect(cid3, "^(L0[0-8]|L9[78]|M86|A4[01]|A46)$") ~ "Infecção/úlcera/osteomielite",
      str_detect(cid3, "^(C[0-9]{2}|D[0-4][0-9])$") ~ "Neoplasia",
      TRUE ~ "Outras causas"),
    cid_revasc_ok = coalesce(str_starts(cid3, "I70") | cid4 %in% CID4_REVASC, FALSE),
    # Conjuntos analíticos
    amp_prim = tipo == "Amputação" & grupo_cid == "Não traumática",
    rev_prim = proc %in% c("rev_aberta", "rev_endo") & cid_revasc_ok,
    rev_sem_cid   = proc %in% c("rev_aberta", "rev_endo"),
    rev_cod_sens  = proc %in% c("rev_aberta", "rev_endo", "rev_sens") & cid_revasc_ok
  )

# ==============================================================================
# 2. Controle de qualidade
# ==============================================================================
dir.create(file.path(PASTA, "resultados"), showWarnings = FALSE)
message("\n---- Controle de qualidade ----")
qc <- tibble(
  item = c("Registros", "Sem idade", "Sem sexo", "VAL_TOT sem valor numérico",
           "DT_INTER inválida", "N_AIH duplicado dentro da UF", "UF do município de movimento difere da UF do arquivo"),
  n = c(nrow(base), sum(is.na(base$idade_anos)), sum(is.na(base$sexo)), sum(is.na(base$val_tot)),
        sum(is.na(base$dt_inter)), sum(duplicated(base[, c("N_AIH", "UF_ARQ")])), NA_integer_))
mapa_uf <- c("11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR", "15" = "PA", "16" = "AP", "17" = "TO")
base$uf_mov <- unname(mapa_uf[str_sub(base$MUNIC_MOV, 1, 2)])
qc$n[7] <- sum(base$uf_mov != base$UF_ARQ, na.rm = TRUE)
print(qc)
write_csv(qc, file.path(PASTA, "resultados", "qc_resumo.csv"))
if (mean(is.na(base$val_tot)) > 0.005)
  stop("VAL_TOT com muitos valores NA: verifique o formato decimal antes de seguir.")
print(summary(base$val_tot))

write_csv(base %>% filter(amp_prim) %>% count(DIAG_PRINC, sort = TRUE) %>% slice_head(n = 40),
          file.path(PASTA, "resultados", "qc_top_cid_amputacao.csv"))
write_csv(as.data.frame(table(UF_arquivo = base$UF_ARQ, UF_movimento = base$uf_mov, useNA = "ifany")),
          file.path(PASTA, "resultados", "qc_uf_arquivo_vs_movimento.csv"))

# ==============================================================================
# 3. Fluxo de exclusão (para o fluxograma)
# ==============================================================================
fluxo_amp <- base %>% filter(tipo == "Amputação") %>%
  count(UF_ARQ, proc, grupo_cid) %>% arrange(UF_ARQ, proc, grupo_cid)
write_csv(fluxo_amp, file.path(PASTA, "resultados", "fluxo_amputacoes.csv"))
fluxo_rev <- base %>% filter(tipo == "Revascularização") %>%
  count(UF_ARQ, proc, cid_revasc_ok) %>% arrange(UF_ARQ, proc)
write_csv(fluxo_rev, file.path(PASTA, "resultados", "fluxo_revascularizacoes.csv"))
message("\nAmputações por grupo de CID (todas as UFs):")
print(base %>% filter(tipo == "Amputação") %>% count(grupo_cid))
message("Amputações não traumáticas (conjunto principal): ", sum(base$amp_prim))
message("Revascularizações com filtro de CID: ", sum(base$rev_prim),
        " | sem filtro: ", sum(base$rev_sem_cid))

# ==============================================================================
# 4. Correção monetária: IPCA (SIDRA, tabela 1737, variável 2266), base dez/2025
# ==============================================================================
ipca_raw <- tryCatch(get_sidra(api = "/t/1737/n1/all/v/2266/p/all"),
                     error = function(e) stop("Falha ao baixar o IPCA do SIDRA: ", conditionMessage(e),
                                              "\nMe envie esta mensagem de erro."))
col_mes <- grep("Mês \\(Código\\)", names(ipca_raw), value = TRUE)[1]
if (is.na(col_mes)) { print(names(ipca_raw)); stop("Coluna de mês não encontrada; me envie os nomes acima.") }
ipca <- tibble(ym = as.integer(ipca_raw[[col_mes]]), indice = as.numeric(ipca_raw$Valor)) %>% arrange(ym)
write_csv(ipca, file.path(PASTA, "resultados", "ipca_sidra_1737.csv"))
indice_base <- ipca$indice[ipca$ym == ANO_BASE_IPCA]
stopifnot("Índice de dez/2025 não encontrado" = length(indice_base) == 1)
message("IPCA (base dez/1993=100) em dez/2025: ", indice_base)

base <- base %>%
  mutate(ym = ano * 100L + mes) %>%
  left_join(ipca, by = "ym") %>%
  mutate(fator_ipca = indice_base / indice,
         custo_corr = val_tot * fator_ipca)
if (any(is.na(base$custo_corr))) warning("Há ", sum(is.na(base$custo_corr)), " AIH sem custo corrigido.")

# ==============================================================================
# 5. Populações (IBGE, Projeções da População, Revisão 2024)
# ==============================================================================
pop_raw <- read_excel(arq_pop, sheet = 1, skip = 5)
names(pop_raw)[1:5] <- c("faixa", "sexo", "cod", "sigla", "local")
pop_long <- pop_raw %>%
  filter(!is.na(faixa)) %>%
  pivot_longer(cols = matches("^[0-9]{4}$"), names_to = "ano", values_to = "pop") %>%
  mutate(ano = as.integer(ano), faixa = str_trim(faixa),
         faixa5 = if_else(faixa %in% c("80-84", "85-89", "90+"), "80+", faixa)) %>%
  filter(sigla %in% c(UFS, "BR"), ano >= 2015, ano <= 2025) %>%
  group_by(sigla, sexo, ano, faixa5) %>%
  summarise(pop = sum(pop), .groups = "drop")
stopifnot("Faixas etárias inesperadas na planilha" = all(unique(pop_long$faixa5) %in% faixas_lv))

pop_amb <- pop_long %>% filter(sexo == "Ambos", sigla %in% UFS)
padrao <- pop_long %>% filter(sigla == "BR", sexo == "Ambos", ano == ANO_PADRAO) %>%
  select(faixa5, pop) %>% arrange(match(faixa5, faixas_lv))
stopifnot("População-padrão incompleta" = nrow(padrao) == length(faixas_lv))
write_csv(pop_long %>% filter(sigla %in% UFS), file.path(PASTA, "resultados", "populacao_norte_2015_2025.csv"))

# ==============================================================================
# 6. Taxas padronizadas por idade (método direto; padrão = Brasil 2022)
# ==============================================================================
agrupamentos <- list(AC = "AC", AM = "AM", AP = "AP", PA = "PA", RO = "RO", RR = "RR", TO = "TO",
                     "Norte sem PA" = c("AC", "AM", "AP", "RO", "RR", "TO"),
                     "Norte" = UFS)

calc_taxas <- function(casos, nome_desfecho) {
  casos <- casos %>% filter(!is.na(faixa5)) %>% mutate(faixa5 = as.character(faixa5))
  purrr::map_dfr(names(agrupamentos), function(g) {
    ufs_g <- agrupamentos[[g]]
    c_g <- casos %>% filter(UF_ARQ %in% ufs_g) %>% count(ano, faixa5, name = "casos")
    p_g <- pop_amb %>% filter(sigla %in% ufs_g) %>% group_by(ano, faixa5) %>%
      summarise(pop = sum(pop), .groups = "drop")
    d <- p_g %>% left_join(c_g, by = c("ano", "faixa5")) %>%
      mutate(casos = coalesce(casos, 0L)) %>% arrange(ano, match(faixa5, faixas_lv))
    purrr::map_dfr(sort(unique(d$ano)), function(a) {
      x <- d %>% filter(ano == a)
      std <- padrao$pop[match(x$faixa5, padrao$faixa5)]
      r <- tryCatch(epitools::ageadjust.direct(count = x$casos, pop = x$pop, stdpop = std),
                    error = function(e) c(crude.rate = NA, adj.rate = NA, lci = NA, uci = NA))
      tibble(grupo = g, desfecho = nome_desfecho, ano = a,
             casos = sum(x$casos), pop = sum(x$pop),
             taxa_bruta = unname(r["crude.rate"]) * 1e5, taxa_pad = unname(r["adj.rate"]) * 1e5,
             lci = unname(r["lci"]) * 1e5, uci = unname(r["uci"]) * 1e5)
    })
  })
}

taxas <- dplyr::bind_rows(
  calc_taxas(base %>% filter(amp_prim),                         "amp_total"),
  calc_taxas(base %>% filter(amp_prim, proc == "amp_maior"),    "amp_maior"),
  calc_taxas(base %>% filter(amp_prim, proc == "amp_pe"),       "amp_pe"),
  calc_taxas(base %>% filter(amp_prim, dx == "Diabetes (E10-E14)"),  "amp_diabetes"),
  calc_taxas(base %>% filter(amp_prim, dx != "Diabetes (E10-E14)"),  "amp_nao_diabetes"),
  calc_taxas(base %>% filter(rev_prim),                         "rev_prim"))
write_csv(taxas, file.path(PASTA, "resultados", "taxas_padronizadas.csv"))

message("\nAmputações não traumáticas sem faixa etária (fora das taxas): ",
        sum(base$amp_prim & is.na(base$faixa5)))
print(taxas %>% filter(desfecho == "amp_total", grupo %in% c("PA", "Norte sem PA"), ano %in% c(2015, 2020, 2025)))

# ==============================================================================
# 7. Salvar a base analítica
# ==============================================================================
saveRDS(base, file.path(PASTA, "resultados", "base_analitica.rds"))
message("\nConcluído. Próximo passo: 03_analise.R")
