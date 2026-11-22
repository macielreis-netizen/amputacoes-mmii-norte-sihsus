# ==============================================================================
# 03_analise.R
# Análises estatísticas, tabelas e figuras.
#
# ENTRADAS (resultados/): base_analitica.rds, taxas_padronizadas.csv
# SAÍDAS: resultados/tabelas_manuscrito.xlsx e csv; figuras/*.png e *.tiff (300 dpi)
#
# PLANO ESTATÍSTICO
#  1. Tendência das taxas padronizadas: regressão de Prais-Winsten (log10 da
#     taxa ~ ano); variação percentual anual (APC) com IC95%; comparação das
#     inclinações Pará x Norte sem Pará (teste z aproximado).
#  2. Sensibilidade para contagens pequenas: Poisson/binomial negativa com
#     offset da população (taxa bruta).
#  3. Custos (corrigidos pelo IPCA para dez/2025): mediana e intervalo
#     interquartil; Mann-Whitney (PA x Norte sem PA); Kruskal-Wallis (7 UFs).
#  4. Mortalidade hospitalar: qui-quadrado de tendência e regressão logística.
#  5. Razão amputação/revascularização: tendência por Prais-Winsten.
# ==============================================================================

pacotes <- c("dplyr", "tidyr", "stringr", "readr", "purrr", "ggplot2", "broom", "writexl", "tibble")
faltando <- setdiff(pacotes, rownames(installed.packages()))
if (length(faltando)) install.packages(faltando)
invisible(lapply(pacotes, library, character.only = TRUE))

PASTA <- getwd()
dir_res <- file.path(PASTA, "resultados"); dir_fig <- file.path(PASTA, "figuras")
dir.create(dir_fig, showWarnings = FALSE)
stopifnot(file.exists(file.path(dir_res, "base_analitica.rds")),
          file.exists(file.path(dir_res, "taxas_padronizadas.csv")))

base  <- readRDS(file.path(dir_res, "base_analitica.rds"))
taxas <- read_csv(file.path(dir_res, "taxas_padronizadas.csv"), show_col_types = FALSE)

UFS <- c("AC", "AM", "AP", "PA", "RO", "RR", "TO")
agrupamentos <- list(AC = "AC", AM = "AM", AP = "AP", PA = "PA", RO = "RO", RR = "RR", TO = "TO",
                     "Norte sem PA" = c("AC", "AM", "AP", "RO", "RR", "TO"), "Norte" = UFS)
base$grupo2 <- if_else(base$UF_ARQ == "PA", "Pará", "Norte sem PA")
amp <- base %>% filter(amp_prim)          # conjunto principal: amputações não traumáticas

# ==============================================================================
# Funções auxiliares
# ==============================================================================
# Regressão de Prais-Winsten (AR1) com y = log10(taxa), x = ano
pw_fit <- function(y, x, tol = 1e-5, maxit = 100) {
  n <- length(y); X <- cbind(1, x)
  b <- solve(t(X) %*% X, t(X) %*% y); rho <- 0
  for (i in seq_len(maxit)) {
    e <- as.vector(y - X %*% b)
    rho_new <- sum(e[-1] * e[-n]) / sum(e[-n]^2)
    w <- sqrt(1 - rho_new^2)
    ys <- c(w * y[1], y[-1] - rho_new * y[-n])
    Xs <- rbind(w * X[1, ], X[-1, , drop = FALSE] - rho_new * X[-n, , drop = FALSE])
    bn <- solve(t(Xs) %*% Xs, t(Xs) %*% ys)
    conv <- max(abs(bn - b)) < tol
    b <- bn; rho <- rho_new
    if (conv) break
  }
  res <- ys - Xs %*% b
  s2 <- sum(res^2) / (n - 2)
  vc <- s2 * solve(t(Xs) %*% Xs)
  list(b = as.numeric(b[2]), se = sqrt(vc[2, 2]), rho = rho, df = n - 2)
}

apc_pw <- function(df, y = "taxa_pad") {
  df <- df %>% arrange(ano) %>% filter(!is.na(.data[[y]]))
  if (nrow(df) < 5 || any(df[[y]] <= 0))
    return(tibble(n_anos = nrow(df), rho = NA_real_, b = NA_real_, se = NA_real_, apc = NA_real_,
                  ic_inf = NA_real_, ic_sup = NA_real_, p = NA_real_,
                  tendencia = "não estimável (valor zero ou poucos anos)"))
  fit <- pw_fit(log10(df[[y]]), df$ano - min(df$ano))
  tc <- qt(0.975, fit$df)
  apc <- (10^fit$b - 1) * 100
  p <- 2 * pt(-abs(fit$b / fit$se), fit$df)
  tibble(n_anos = nrow(df), rho = fit$rho, b = fit$b, se = fit$se, apc = apc,
         ic_inf = (10^(fit$b - tc * fit$se) - 1) * 100,
         ic_sup = (10^(fit$b + tc * fit$se) - 1) * 100,
         p = p,
         tendencia = dplyr::case_when(p < 0.05 & apc > 0 ~ "crescente",
                                      p < 0.05 & apc < 0 ~ "decrescente",
                                      TRUE ~ "estacionária"))
}

fmt_p <- function(p) ifelse(is.na(p), NA_character_, ifelse(p < 0.001, "<0,001", sprintf("%.3f", p)))

# ==============================================================================
# 1. Descrição
# ==============================================================================
resumo <- function(df, grupo_var) {
  df %>% group_by(grupo = .data[[grupo_var]]) %>%
    summarise(n_aih = n(),
              pct_maior = 100 * mean(proc == "amp_maior"),
              idade_mediana = median(idade_anos, na.rm = TRUE),
              idade_q1 = quantile(idade_anos, 0.25, na.rm = TRUE),
              idade_q3 = quantile(idade_anos, 0.75, na.rm = TRUE),
              pct_homens = 100 * mean(sexo == "Homens", na.rm = TRUE),
              pct_urgencia = 100 * mean(car_int3 == "Urgência (02)"),
              pct_diabetes = 100 * mean(dx == "Diabetes (E10-E14)"),
              pct_vascular = 100 * mean(dx == "Doença vascular (I70-I79, I96)"),
              pct_gangrena_inesp = 100 * mean(dx == "Gangrena/infecção necrosante inespecífica (R02, A48)"),
              obitos = sum(obito), letalidade_pct = 100 * mean(obito),
              custo_mediano = median(custo_corr, na.rm = TRUE),
              custo_q1 = quantile(custo_corr, 0.25, na.rm = TRUE),
              custo_q3 = quantile(custo_corr, 0.75, na.rm = TRUE),
              custo_total = sum(custo_corr, na.rm = TRUE),
              perm_mediana = median(dias_perm, na.rm = TRUE),
              .groups = "drop")
}
tab_desc <- bind_rows(resumo(amp, "UF_ARQ"), resumo(amp, "grupo2")) %>% mutate(across(where(is.numeric), ~ round(.x, 2)))
tab_dx <- amp %>% count(grupo2, dx) %>% group_by(grupo2) %>% mutate(pct = round(100 * n / sum(n), 1)) %>% ungroup()
# Desfechos por grupo diagnóstico (diabetes x demais causas), PA vs Norte sem PA
amp <- amp %>% mutate(dx2 = case_when(dx == "Diabetes (E10-E14)" ~ "Diabetes (CID principal)",
                                      dx == "Gangrena/infecção necrosante inespecífica (R02, A48)" ~ "Gangrena inespecífica (R02, A48)",
                                      TRUE ~ "Demais causas"))
tab_dx_desf <- amp %>% group_by(grupo2, dx2) %>%
  summarise(n = n(), obitos = sum(obito), letalidade_pct = round(100 * mean(obito), 1),
            custo_mediana = median(custo_corr, na.rm = TRUE),
            custo_q1 = quantile(custo_corr, .25, na.rm = TRUE), custo_q3 = quantile(custo_corr, .75, na.rm = TRUE),
            perm_mediana = median(dias_perm, na.rm = TRUE), .groups = "drop")
print(tab_dx_desf, width = Inf)
tab_dx_ano <- amp %>% count(grupo2, ano, dx2) %>% group_by(grupo2, ano) %>% mutate(pct = round(100 * n / sum(n), 1)) %>% ungroup()
tab_t87 <- base %>% filter(grupo_cid == "T87 (complicação de coto)") %>% count(UF_ARQ, ano)
message("\n==== Descrição por UF e Pará x Norte sem PA ===="); print(tab_desc, width = Inf)

# ==============================================================================
# 2. Tendência das taxas padronizadas (Prais-Winsten)
# ==============================================================================
apc_tab <- taxas %>% group_by(desfecho, grupo) %>% group_modify(~ apc_pw(.x, "taxa_pad")) %>% ungroup()

comp_pa <- apc_tab %>% filter(grupo %in% c("PA", "Norte sem PA")) %>%
  select(desfecho, grupo, b, se) %>%
  pivot_wider(names_from = grupo, values_from = c(b, se)) %>%
  mutate(z = (b_PA - `b_Norte sem PA`) / sqrt(se_PA^2 + `se_Norte sem PA`^2),
         p_diferenca_inclinacao = 2 * pnorm(-abs(z)))

# Sensibilidade: Poisson / binomial negativa com offset da população (taxa bruta)
mod_cont <- function(df) {
  df <- df %>% filter(pop > 0) %>% mutate(t = ano - 2015)
  m <- glm(casos ~ t + offset(log(pop)), family = poisson, data = df)
  disp <- sum(residuals(m, "pearson")^2) / m$df.residual
  modelo <- "Poisson"
  if (disp > 1.5) {
    m2 <- tryCatch(MASS::glm.nb(casos ~ t + offset(log(pop)), data = df), error = function(e) NULL)
    if (!is.null(m2)) { m <- m2; modelo <- "binomial negativa" } else modelo <- "Poisson (superdispersão)"
  }
  co <- summary(m)$coefficients[2, ]; ci <- confint.default(m)[2, ]
  tibble(modelo = modelo, dispersao = disp,
         var_anual_pct = (exp(co[1]) - 1) * 100,
         ic_inf = (exp(ci[1]) - 1) * 100, ic_sup = (exp(ci[2]) - 1) * 100, p = co[4])
}
sens_pois <- taxas %>% filter(desfecho == "amp_total") %>%
  group_by(grupo) %>% group_modify(~ mod_cont(.x)) %>% ungroup()

message("\n==== APC (Prais-Winsten), taxa padronizada ===="); print(apc_tab %>% filter(desfecho == "amp_total"), width = Inf)
message("\n==== Pará x Norte sem PA: diferença de inclinação ===="); print(comp_pa, width = Inf)
message("\n==== Sensibilidade (Poisson/NB) ===="); print(sens_pois, width = Inf)

# ==============================================================================
# 3. Custos (valores corrigidos pelo IPCA para dez/2025)
# ==============================================================================
custo_ano <- amp %>% group_by(grupo2, ano) %>%
  summarise(n = n(), mediana = median(custo_corr, na.rm = TRUE),
            q1 = quantile(custo_corr, 0.25, na.rm = TRUE), q3 = quantile(custo_corr, 0.75, na.rm = TRUE),
            total = sum(custo_corr, na.rm = TRUE), .groups = "drop")
apc_custo <- custo_ano %>% group_by(grupo2) %>% group_modify(~ apc_pw(.x, "mediana")) %>% ungroup()
t_mw <- wilcox.test(custo_corr ~ grupo2, data = amp, conf.int = TRUE)
t_kw <- kruskal.test(custo_corr ~ factor(UF_ARQ), data = amp)
custo_proc <- amp %>% group_by(grupo2, proc) %>%
  summarise(n = n(), mediana = median(custo_corr, na.rm = TRUE),
            q1 = quantile(custo_corr, 0.25, na.rm = TRUE), q3 = quantile(custo_corr, 0.75, na.rm = TRUE), .groups = "drop")
testes_custo <- tibble(teste = c("Mann-Whitney (PA x Norte sem PA)", "Kruskal-Wallis (7 UFs)"),
                       estatistica = c(unname(t_mw$statistic), unname(t_kw$statistic)),
                       p = c(t_mw$p.value, t_kw$p.value))
message("\n==== Custos ===="); print(testes_custo); print(apc_custo, width = Inf)

# ==============================================================================
# 4. Mortalidade hospitalar
# ==============================================================================
let_ano <- amp %>% group_by(grupo2, ano) %>%
  summarise(n = n(), obitos = sum(obito), letalidade_pct = 100 * obitos / n, .groups = "drop")
tend_let <- let_ano %>% group_by(grupo2) %>%
  group_modify(~ {
    r <- prop.trend.test(x = .x$obitos, n = .x$n, score = .x$ano)
    tibble(qui2_tendencia = unname(r$statistic), p = r$p.value)
  }) %>% ungroup()
let_uf <- amp %>% group_by(UF_ARQ) %>% summarise(n = n(), obitos = sum(obito), letalidade_pct = 100 * mean(obito), .groups = "drop")
let_proc <- amp %>% group_by(grupo2, proc) %>% summarise(n = n(), obitos = sum(obito), letalidade_pct = 100 * mean(obito), .groups = "drop")

amp_m <- amp %>% filter(!is.na(idade_anos), !is.na(sexo)) %>%
  mutate(pa = factor(grupo2, levels = c("Norte sem PA", "Pará")),
         idade10 = idade_anos / 10, ano_c = ano - 2015,
         proc = factor(proc), sexo = factor(sexo), car_int3 = factor(car_int3), dx = factor(dx))
m_log <- glm(obito ~ pa + idade10 + sexo + proc + car_int3 + dx + ano_c, family = binomial, data = amp_m)
or_tab <- broom::tidy(m_log, conf.int = TRUE, exponentiate = TRUE) %>% mutate(across(where(is.numeric), ~ round(.x, 4)))
# Sensibilidade (C): sem ajuste por grupo diagnóstico, pois a codificação do CID principal varia entre UFs e no tempo
m_log_sem_dx <- glm(obito ~ pa + idade10 + sexo + proc + car_int3 + ano_c, family = binomial, data = amp_m)
or_tab_sem_dx <- broom::tidy(m_log_sem_dx, conf.int = TRUE, exponentiate = TRUE) %>% mutate(across(where(is.numeric), ~ round(.x, 4)))
amp_m$UF_ARQ <- relevel(factor(amp_m$UF_ARQ), ref = "PA")
m_log_uf <- glm(obito ~ UF_ARQ + idade10 + sexo + proc + car_int3 + dx + ano_c, family = binomial, data = amp_m)
or_tab_uf <- broom::tidy(m_log_uf, conf.int = TRUE, exponentiate = TRUE) %>% mutate(across(where(is.numeric), ~ round(.x, 4)))
message("\n==== Mortalidade ===="); print(tend_let); print(or_tab %>% filter(term == "paPará"))
message("Sensibilidade sem ajuste por diagnóstico:"); print(or_tab_sem_dx %>% filter(term == "paPará"))

# ==============================================================================
# 5. Razão amputação / revascularização (por UF e ano, local de internação)
# ==============================================================================
cont_g <- function(df, nome) {
  purrr::map_dfr(names(agrupamentos), function(g) {
    x <- df %>% filter(UF_ARQ %in% agrupamentos[[g]]) %>% count(ano)
    tibble(ano = 2015:2025) %>% left_join(x, by = "ano") %>%
      mutate(n = coalesce(n, 0L), grupo = g, serie = nome)
  })
}
cont <- bind_rows(cont_g(base %>% filter(amp_prim), "amp_total"),
                  cont_g(base %>% filter(amp_prim, proc == "amp_maior"), "amp_maior"),
                  cont_g(base %>% filter(rev_prim), "rev_prim"),
                  cont_g(base %>% filter(rev_sem_cid), "rev_sem_cid"),
                  cont_g(base %>% filter(rev_cod_sens), "rev_cod_sens")) %>%
  pivot_wider(names_from = serie, values_from = n)

razao <- cont %>% transmute(grupo, ano, amp_total, amp_maior, rev_prim,
                            razao_principal = amp_total / rev_prim,
                            razao_maior_rev = amp_maior / rev_prim,
                            razao_sem_cid = amp_total / rev_sem_cid,
                            razao_cod_sens = amp_total / rev_cod_sens)
razao[sapply(razao, is.numeric)] <- lapply(razao[sapply(razao, is.numeric)], function(v) replace(v, is.infinite(v) | is.nan(v), NA))
apc_razao <- bind_rows(
  razao %>% group_by(grupo) %>% group_modify(~ apc_pw(.x, "razao_principal")) %>% mutate(serie = "razao_principal"),
  razao %>% group_by(grupo) %>% group_modify(~ apc_pw(.x, "razao_maior_rev")) %>% mutate(serie = "razao_maior_rev"),
  razao %>% group_by(grupo) %>% group_modify(~ apc_pw(.x, "razao_sem_cid")) %>% mutate(serie = "razao_sem_cid"),
  razao %>% group_by(grupo) %>% group_modify(~ apc_pw(.x, "razao_cod_sens")) %>% mutate(serie = "razao_cod_sens")) %>%
  ungroup()
message("\n==== Razão amputação/revascularização (principal) ====")
print(apc_razao %>% filter(serie == "razao_principal"), width = Inf)

# ==============================================================================
# 6. Figuras (300 dpi; PNG e TIFF)
# ==============================================================================
salvar <- function(p, nome, w = 18, h = 11) {
  ggsave(file.path(dir_fig, paste0(nome, ".png")), p, width = w, height = h, units = "cm", dpi = 300)
  ggsave(file.path(dir_fig, paste0(nome, ".tiff")), p, width = w, height = h, units = "cm", dpi = 300, compression = "lzw")
}
tema <- theme_bw(base_size = 10) + theme(legend.position = "bottom", legend.title = element_blank())

d1 <- taxas %>% filter(desfecho == "amp_total", grupo %in% c("PA", "Norte sem PA")) %>%
  mutate(grupo = recode(grupo, PA = "Pará"))
f1 <- ggplot(d1, aes(ano, taxa_pad, colour = grupo, fill = grupo)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.15, colour = NA) + geom_line(linewidth = 0.8) + geom_point(size = 1.5) +
  scale_x_continuous(breaks = 2015:2025) +
  labs(x = "Ano de competência", y = "Taxa padronizada por idade (por 100 mil hab.)") + tema
salvar(f1, "fig1_taxa_padronizada_PA_vs_Norte")

d1b <- taxas %>% filter(desfecho == "amp_total", grupo %in% UFS)
f1b <- ggplot(d1b, aes(ano, taxa_pad, colour = grupo)) + geom_line(linewidth = 0.7) + geom_point(size = 1.2) +
  scale_x_continuous(breaks = 2015:2025) +
  labs(x = "Ano de competência", y = "Taxa padronizada por idade (por 100 mil hab.)") + tema
salvar(f1b, "fig1b_taxa_padronizada_por_UF")

f2 <- ggplot(custo_ano, aes(ano, mediana, colour = grupo2, fill = grupo2)) +
  geom_ribbon(aes(ymin = q1, ymax = q3), alpha = 0.12, colour = NA) + geom_line(linewidth = 0.8) + geom_point(size = 1.5) +
  scale_x_continuous(breaks = 2015:2025) +
  labs(x = "Ano de competência", y = "Custo mediano por AIH (R$ de dez/2025; faixa = intervalo interquartil)") + tema
salvar(f2, "fig2_custo_mediano_corrigido")

f3 <- ggplot(let_ano, aes(ano, letalidade_pct, colour = grupo2)) + geom_line(linewidth = 0.8) + geom_point(size = 1.5) +
  scale_x_continuous(breaks = 2015:2025) +
  labs(x = "Ano de competência", y = "Mortalidade hospitalar (%)") + tema
salvar(f3, "fig3_mortalidade_hospitalar")

f4 <- ggplot(razao %>% filter(grupo %in% c("PA", "Norte sem PA")), aes(ano, razao_principal, colour = grupo)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.5) + scale_x_continuous(breaks = 2015:2025) +
  labs(x = "Ano de competência", y = "Razão amputação / revascularização de MMII") + tema
salvar(f4, "fig4_razao_amputacao_revascularizacao")

# ==============================================================================
# 7. Exportar tabelas
# ==============================================================================
write_xlsx(list(
  descricao = tab_desc, diagnosticos = tab_dx, desfechos_por_dx = tab_dx_desf, dx_por_ano = tab_dx_ano, T87_separado = tab_t87,
  taxas = taxas, APC = apc_tab, dif_inclinacao_PA_vs_NorteSemPA = comp_pa, sensibilidade_poisson = sens_pois,
  custo_ano = custo_ano, custo_APC = apc_custo, custo_testes = testes_custo, custo_procedimento = custo_proc,
  letalidade_ano = let_ano, letalidade_tendencia = tend_let, letalidade_UF = let_uf, letalidade_proc = let_proc,
  OR_PA_vs_Norte = or_tab, OR_PA_sem_dx = or_tab_sem_dx, OR_por_UF = or_tab_uf,
  contagens = cont, razao = razao, razao_APC = apc_razao),
  file.path(dir_res, "tabelas_manuscrito.xlsx"))
capture.output(sessionInfo(), file = file.path(dir_res, "sessionInfo.txt"))
message("\nConcluído. Tabelas: resultados/tabelas_manuscrito.xlsx | Figuras: pasta figuras/")
