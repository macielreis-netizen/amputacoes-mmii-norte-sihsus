# ==============================================================================
# 01_extracao.R
# Tendência temporal, custos e mortalidade hospitalar por amputações não
# traumáticas dos membros inferiores no Pará e nos demais estados do Norte,
# 2015-2025. Fonte: SIH/SUS (SIH-RD, AIH aprovadas), via pacote microdatasus.
#
# O QUE ESTE SCRIPT FAZ
#   Baixa mês a mês (7 UFs x 11 anos x 12 meses), mantém só as AIH cujo
#   procedimento realizado (PROC_REA) está na lista de interesse, aplica
#   process_sih() apenas nesse subconjunto e salva um arquivo por UF-ano.
#   Pode ser interrompido e reiniciado: UF-ano já salvo é pulado.
#
# COMO USAR
#   1. Abra este arquivo no RStudio.
#   2. Menu: Session > Set Working Directory > To Source File Location
#      (a pasta do projeto, onde também estará o xlsx do IBGE).
#   3. Rode tudo (Cmd+Shift+Enter ou botão Source). A primeira execução é longa.
# ==============================================================================

pacotes <- c("microdatasus", "dplyr", "purrr", "readr", "stringr")
faltando <- setdiff(pacotes, rownames(installed.packages()))
if (length(faltando)) install.packages(faltando)
invisible(lapply(pacotes, library, character.only = TRUE))

PASTA <- getwd()
message("Pasta do projeto: ", PASTA)
dir.create(file.path(PASTA, "dados_brutos"), showWarnings = FALSE)
dir.create(file.path(PASTA, "resultados"),   showWarnings = FALSE)

# ---- Parâmetros do estudo -----------------------------------------------------
ANOS  <- 2015:2025                                  # ano de competência (ANO_CMPT)
MESES <- 1:12
UFS   <- c("AC", "AM", "AP", "PA", "RO", "RR", "TO")

# Amputações de MMII (SIGTAP, formato SIH de 10 dígitos)
COD_AMP_MAIOR <- "0408050012"   # amputação/desarticulação de MMII (perna, joelho, coxa, quadril)
COD_AMP_PE    <- "0408050020"   # amputação/desarticulação de pé e tarso

# Revascularização de MMII, cirurgia aberta
COD_REV_ABERTA <- c("0406020329",  # ponte axilo-femoral
                    "0406020345", "0406020353", "0406020361", "0406020388",
                    "0406020434", "0406020442", "0406020450")
# Revascularização de MMII, endovascular
COD_REV_ENDO <- c("0406040028", "0406040044", "0406040052",
                  "0406040060", "0406040079", "0406040281")
# Códigos usados só na análise de sensibilidade
COD_REV_SENS <- c("0406020124", "0406020302", "0406020396")

COD_TODOS <- c(COD_AMP_MAIOR, COD_AMP_PE, COD_REV_ABERTA, COD_REV_ENDO, COD_REV_SENS)

# Colunas mantidas (as que não existirem em algum ano são ignoradas)
COLUNAS <- c("N_AIH", "UF_ZI", "ANO_CMPT", "MES_CMPT", "DT_INTER", "DT_SAIDA",
             "PROC_REA", "PROC_SOLIC", "DIAG_PRINC", "DIAG_SECUN",
             "MUNIC_RES", "MUNIC_MOV", "CNES", "NAT_JUR", "COMPLEX", "ESPEC",
             "IDADE", "COD_IDADE", "SEXO", "RACA_COR",
             "CAR_INT", "MORTE", "DIAS_PERM", "UTI_INT_TO",
             "VAL_TOT", "VAL_SH", "VAL_SP", "VAL_UTI")

# ---- Função de download com novas tentativas ---------------------------------
baixar_mes <- function(uf, ano, mes, tentativas = 3) {
  for (k in seq_len(tentativas)) {
    res <- tryCatch(
      suppressMessages(
        fetch_datasus(year_start = ano, month_start = mes,
                      year_end = ano, month_end = mes,
                      uf = uf, information_system = "SIH-RD",
                      stop_on_error = FALSE, timeout = 240, quiet = TRUE)),
      error = function(e) e)
    if (!inherits(res, "error") && !is.null(res) && nrow(res) > 0) return(res)
    Sys.sleep(5)
  }
  NULL
}

# ---- Laço principal: UF x ano x mês ------------------------------------------
for (uf in UFS) {
  for (ano in ANOS) {
    arq <- file.path(PASTA, "dados_brutos", sprintf("SIH_%s_%d.rds", uf, ano))
    if (file.exists(arq)) { message("[pulado, ja existe] ", basename(arq)); next }

    partes <- list(); logs <- list()
    for (mes in MESES) {
      bruto <- baixar_mes(uf, ano, mes)
      if (is.null(bruto)) {
        logs[[length(logs) + 1]] <- data.frame(uf = uf, ano = ano, mes = mes,
                                               n_total = NA, n_filtrado = NA, status = "FALHA")
        message("  FALHA: ", uf, " ", ano, "-", sprintf("%02d", mes))
        next
      }
      n_total <- nrow(bruto)
      filtrado <- bruto[as.character(bruto$PROC_REA) %in% COD_TODOS, , drop = FALSE]
      n_filtrado <- nrow(filtrado)
      if (n_filtrado > 0) {
        filtrado <- process_sih(filtrado)
        filtrado <- filtrado[, intersect(COLUNAS, names(filtrado)), drop = FALSE]
        filtrado[] <- lapply(filtrado, as.character)   # evita conflito de fatores entre meses
        filtrado$UF_ARQ <- uf                          # UF do arquivo = UF de internação
        partes[[length(partes) + 1]] <- filtrado
      }
      logs[[length(logs) + 1]] <- data.frame(uf = uf, ano = ano, mes = mes,
                                             n_total = n_total, n_filtrado = n_filtrado, status = "ok")
      rm(bruto, filtrado)
    }

    log_ufano <- dplyr::bind_rows(logs)
    saveRDS(log_ufano, file.path(PASTA, "dados_brutos", sprintf("log_%s_%d.rds", uf, ano)))

    if (any(log_ufano$status != "ok")) {
      warning(uf, " ", ano, ": houve meses com falha; o arquivo NAO foi salvo. ",
              "Rode o script de novo para tentar outra vez.")
      next
    }
    saveRDS(dplyr::bind_rows(partes), arq)
    message("[salvo] ", basename(arq), " | AIH filtradas: ", sum(log_ufano$n_filtrado))
  }
}

# ---- Consolidação ------------------------------------------------------------
arqs <- list.files(file.path(PASTA, "dados_brutos"), pattern = "^SIH_.*\\.rds$", full.names = TRUE)
esperado <- length(UFS) * length(ANOS)
if (length(arqs) < esperado) {
  warning("Faltam ", esperado - length(arqs), " arquivos UF-ano. Rode o script de novo antes de seguir.")
}

base <- dplyr::bind_rows(lapply(arqs, readRDS))
saveRDS(base, file.path(PASTA, "resultados", "sih_filtrado_bruto.rds"))

logs_all <- dplyr::bind_rows(lapply(
  list.files(file.path(PASTA, "dados_brutos"), pattern = "^log_.*\\.rds$", full.names = TRUE), readRDS))
write_csv(logs_all, file.path(PASTA, "resultados", "log_extracao.csv"))

message("\n==== RESUMO DA EXTRACAO ====")
message("Arquivos UF-ano salvos: ", length(arqs), " de ", esperado)
message("Meses com falha: ", sum(logs_all$status != "ok", na.rm = TRUE))
message("AIH totais lidas (todas as causas): ", format(sum(logs_all$n_total, na.rm = TRUE), big.mark = "."))
message("AIH com procedimento de interesse: ", format(nrow(base), big.mark = "."))
print(table(base$UF_ARQ, base$PROC_REA))
message("\nProximo passo: rode 02_preparo.R")
