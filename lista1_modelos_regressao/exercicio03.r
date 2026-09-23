# 3. Conjunto de Dados do Exercicio 3
# X: Investimento em publicidade (em milhares de reais)
# Y: Volume de vendas (em milhares de unidades)
investimento <- c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55)
vendas <- c(25, 32, 38, 45, 51, 57, 63, 68, 74, 79)
dados <- data.frame(Investimento = investimento, Vendas = vendas)

n <- nrow(dados)

# ==============================================================================
# a) Calcule o coeficiente de correlacao linear de Pearson e verifique se ele e significativo.
# ==============================================================================

media_x <- mean(dados$Investimento)
media_y <- mean(dados$Vendas)

Sxx <- sum((dados$Investimento - media_x)^2)
Syy <- sum((dados$Vendas - media_y)^2)
Sxy <- sum((dados$Investimento - media_x) * (dados$Vendas - media_y))

cat("--- Item a: Somas de Quadrados e Correlacao ---\n")
cat("Tamanho amostral (n):", n, "\n")
cat("Media de X (Investimento):", media_x, "\n")
cat("Media de Y (Vendas):", media_y, "\n")
cat("Sxx:", Sxx, "\n")
cat("Syy:", Syy, "\n")
cat("Sxy:", Sxy, "\n")

# Coeficiente de correlacao de Pearson manual:
r <- Sxy / sqrt(Sxx * Syy)
cat("r (manual):", r, "\n\n")

# Teste de significancia (H0: rho = 0 vs H1: rho != 0):
teste_cor <- cor.test(dados$Investimento, dados$Vendas, method = "pearson")
print(teste_cor)

# ==============================================================================
# b) Encontre a equacao da reta de regressao.
# ==============================================================================

# Estimadores de MQO (Minimos Quadrados Ordinarios):
# beta1_hat = Sxy / Sxx
# beta0_hat = media_y - beta1_hat * media_x
beta1_hat <- Sxy / Sxx
beta0_hat <- media_y - beta1_hat * media_x

cat("\n--- Item b: Coeficientes da Reta de Regressao ---\n")
cat("beta1 (inclinacao):", beta1_hat, "\n")
cat("beta0 (intercepto):", beta0_hat, "\n")
cat(sprintf("Equacao da reta: Vendas_chapeu = %.4f + %.4f * Investimento\n\n", beta0_hat, beta1_hat))

# Ajuste do modelo via funcao lm() do R:
modelo <- lm(Vendas ~ Investimento, data = dados)
cat("Resumo do modelo ajustado via lm():\n")
print(summary(modelo))

# ==============================================================================
# c) Interprete os coeficientes beta0 e beta1 no contexto do problema.
# ==============================================================================

# Interpretacao de beta0 (Intercepto = 14.2):
# Representa o volume medio de vendas esperado quando nao ha investimento em publicidade (X = 0).
# No contexto: sem investimento em publicidade, espera-se uma venda basal de aproximadamente
# 14.20 milhares de unidades (ou 14.200 unidades).

# Interpretacao de beta1 (Inclinacao = 1.2):
# Representa a taxa media de variacao nas vendas para cada unidade adicional investida.
# No contexto: para cada R$ 1.000,00 adicionais investidos em publicidade (1 unidade de X),
# espera-se um aumento medio de 1.20 milhares de unidades vendidas (1.200 unidades).

# ==============================================================================
# d) Construa a ANOVA e verifique se a regressao pode ser considerada significativa.
# ==============================================================================

SQ_Reg <- beta1_hat * Sxy
SQ_Tot <- Syy
SQ_Res <- SQ_Tot - SQ_Reg

gl_reg <- 1
gl_res <- n - 2
gl_tot <- n - 1

QM_Reg <- SQ_Reg / gl_reg
QM_Res <- SQ_Res / gl_res

F0 <- QM_Reg / QM_Res
p_valor_F <- 1 - pf(F0, df1 = gl_reg, df2 = gl_res)
F_crit <- qf(0.95, df1 = gl_reg, df2 = gl_res)

cat("\n--- Item d: Tabela ANOVA Manual ---\n")
cat(sprintf("Regressao: SQ = %.4f | GL = %d | QM = %.4f | F = %.2f | p-valor = %e\n", SQ_Reg, gl_reg, QM_Reg, F0, p_valor_F))
cat(sprintf("Residuos:  SQ = %.4f | GL = %d | QM = %.4f\n", SQ_Res, gl_res, QM_Res))
cat(sprintf("Total:     SQ = %.4f | GL = %d\n", SQ_Tot, gl_tot))
cat(sprintf("F critico (alpha = 0.05): %.4f\n\n", F_crit))

# ANOVA direta no R:
cat("Tabela ANOVA via anova(modelo):\n")
tabela_anova <- anova(modelo)
print(tabela_anova)

# ==============================================================================
# e) Qual o coeficiente de determinacao R^2 desse estudo. Interprete.
# ==============================================================================

R2 <- SQ_Reg / SQ_Tot
cat("\n--- Item e: Coeficiente de Determinacao R^2 ---\n")
cat(sprintf("R^2 (calculado): %.4f (ou %.2f%%)\n", R2, R2 * 100))
cat(sprintf("R^2 (via summary): %.4f\n\n", summary(modelo)$r.squared))

# Interpretacao:
# Cerca de 99.81% da variabilidade total no volume de vendas e explicada pelo
# investimento em publicidade atraves do modelo linear ajustado. Os restantes 0.19%
# sao atribuidos a residuos / erros aleatorios.

# ==============================================================================
# f) Construa o intervalo de 95% de confianca para os parametros.
# ==============================================================================

alpha <- 0.05
t_crit <- qt(1 - alpha / 2, df = gl_res)

ep_beta1 <- sqrt(QM_Res / Sxx)
ep_beta0 <- sqrt(QM_Res * (1 / n + (media_x^2) / Sxx))

ic_beta0 <- c(beta0_hat - t_crit * ep_beta0, beta0_hat + t_crit * ep_beta0)
ic_beta1 <- c(beta1_hat - t_crit * ep_beta1, beta1_hat + t_crit * ep_beta1)

cat("--- Item f: Intervalos de 95% de Confianca ---\n")
cat(sprintf("Valor critico t (gl = %d, alpha = 0.05): %.4f\n", gl_res, t_crit))
cat(sprintf("IC 95%% para beta0 (Intercepto): [%.4f, %.4f]\n", ic_beta0[1], ic_beta0[2]))
cat(sprintf("IC 95%% para beta1 (Inclinacao):  [%.4f, %.4f]\n\n", ic_beta1[1], ic_beta1[2]))

cat("Intervalos via confint(modelo, level = 0.95):\n")
print(confint(modelo, level = 0.95))

# ==============================================================================
# g) Qual seria o volume de vendas previsto para um investimento de R$ 42.000,00?
# ==============================================================================

# Como X esta em milhares de reais, R$ 42.000,00 equivale a X0 = 42.
x0 <- 42
vendas_previstas <- beta0_hat + beta1_hat * x0

# Erro padrao e Intervalo de Predicao para um mes/caso individual:
ep_pred <- sqrt(QM_Res * (1 + 1 / n + ((x0 - media_x)^2) / Sxx))
ic_pred <- c(vendas_previstas - t_crit * ep_pred, vendas_previstas + t_crit * ep_pred)

# Erro padrao e Intervalo de Confianca para a media:
ep_media <- sqrt(QM_Res * (1 / n + ((x0 - media_x)^2) / Sxx))
ic_media <- c(vendas_previstas - t_crit * ep_media, vendas_previstas + t_crit * ep_media)

cat("\n--- Item g: Previsao de Vendas para Investimento de R$ 42.000,00 (X = 42) ---\n")
cat(sprintf("Vendas previstas pontual: %.4f mil unidades (%.0f unidades)\n", vendas_previstas, vendas_previstas * 1000))
cat(sprintf("Intervalo de 95%% de Predicao (caso individual): [%.4f, %.4f] mil unidades\n", ic_pred[1], ic_pred[2]))
cat(sprintf("Intervalo de 95%% de Confianca (para a media):     [%.4f, %.4f] mil unidades\n\n", ic_media[1], ic_media[2]))

novo_investimento <- data.frame(Investimento = 42)
cat("Via predict(interval = 'prediction'):\n")
print(predict(modelo, newdata = novo_investimento, interval = "prediction", level = 0.95))

# ==============================================================================
# h) Para aumentar as vendas em 5.000 unidades, quanto aproximadamente devera ser aumentado o investimento em publicidade?
# ==============================================================================

# 5.000 unidades equivalem a Delta_Y = 5 (em milhares de unidades).
# Pela equacao do modelo: Delta_Y = beta1 * Delta_X
# Logo: Delta_X = Delta_Y / beta1
delta_vendas <- 5
delta_investimento <- delta_vendas / beta1_hat
delta_investimento_reais <- delta_investimento * 1000

cat("\n--- Item h: Investimento Necessario para Aumento de 5.000 Unidades nas Vendas ---\n")
cat(sprintf("Aumento desejado nas vendas (Delta Y): %.1f mil unidades\n", delta_vendas))
cat(sprintf("Aumento necessario no investimento (Delta X): %.4f mil reais\n", delta_investimento))
cat(sprintf("Em reais: R$ %.2f\n", delta_investimento_reais))

