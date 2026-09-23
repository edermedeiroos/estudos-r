# 1. Conjunto de Dados do Exercício 1
consumo <- c(7, 6, 5, 4, 3, 2, 1, 3, 2, 1, 0, 2, 5, 4, 5, 4, 6, 3, 7, 1)
imc <- c(30.1, 28.8, 27.5, 26.2, 25.0, 24.5, 23.8, 25.0, 24.5, 23.8, 22.5, 24.0, 27.0, 26.5, 27.5, 26.2, 28.8, 24.8, 29.9, 23.5)
dados <- data.frame(Consumo = consumo, IMC = imc)

n <- nrow(dados)

# ==============================================================================
# a) Coeficiente de correlacao linear de Pearson e teste de significancia
# ==============================================================================

Sxx <- sum((dados$Consumo - mean(dados$Consumo))^2)
Syy <- sum((dados$IMC - mean(dados$IMC))^2)
Sxy <- sum((dados$Consumo - mean(dados$Consumo)) * (dados$IMC - mean(dados$IMC)))

cat("--- Item a: Somas de Quadrados e Correlacao ---\n")
cat("Sxx:", Sxx, "\n")
cat("Syy:", Syy, "\n")
cat("Sxy:", Sxy, "\n")

r <- Sxy / sqrt(Sxx * Syy)
cat("r (manual):", r, "\n\n")

teste_cor <- cor.test(dados$Consumo, dados$IMC, method = "pearson")
print(teste_cor)

# ==============================================================================
# b) Encontre a equacao da reta de regressao
# ==============================================================================

# Calculo manual dos estimadores de MQO (Minimos Quadrados Ordinarios):
media_x <- mean(dados$Consumo)
media_y <- mean(dados$IMC)

beta1_hat <- Sxy / Sxx
beta0_hat <- media_y - beta1_hat * media_x

cat("\n--- Item b: Coeficientes da Reta de Regressao ---\n")
cat("Media de X (Consumo):", media_x, "\n")
cat("Media de Y (IMC):", media_y, "\n")
cat("beta1 (inclinacao):", beta1_hat, "\n")
cat("beta0 (intercepto):", beta0_hat, "\n")
cat(sprintf("Equacao da reta: IMC_chapeu = %.4f + %.4f * Consumo\n\n", beta0_hat, beta1_hat))

# Ajuste do modelo via funcao lm() do R:
modelo <- lm(IMC ~ Consumo, data = dados)
cat("Resumo do modelo ajustado via lm():\n")
print(summary(modelo))

# ==============================================================================
# c) Interprete os coeficientes beta0 e beta1 no contexto do problema
# ==============================================================================

# Interpretacao de beta0 (Intercepto = 22.2693):
# Representa o valor esperado do IMC quando o consumo semanal de fast-food e zero (X = 0).
# No contexto: para um paciente que nao consome fast-food durante a semana,
# o IMC medio estimado e de aproximadamente 22.27 kg/m^2.
# Nota: Possui sentido pratico pois X = 0 esta presente na amostra observada (Paciente 11).

# Interpretacao de beta1 (Inclinacao = 1.0495):
# Representa a taxa de variacao esperada no IMC para cada unidade adicional no consumo semanal.
# No contexto: para cada vez a mais por semana que o paciente consome fast-food,
# espera-se um aumento medio de aproximadamente 1.05 kg/m^2 no seu IMC.

# ==============================================================================
# d) Construa a ANOVA e verifique se a regressao pode ser considerada significativa
# ==============================================================================

# Calculos manuais da ANOVA:
# SQ_Reg = beta1_hat * Sxy = (Sxy^2) / Sxx
# SQ_Tot = Syy
# SQ_Res = SQ_Tot - SQ_Reg
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

cat("\n--- Item d: Tabela ANOVA Manual ---\n")
cat(sprintf("Regressao: SQ = %.4f | GL = %d | QM = %.4f | F = %.2f | p-valor = %e\n", SQ_Reg, gl_reg, QM_Reg, F0, p_valor_F))
cat(sprintf("Residuos:  SQ = %.4f | GL = %d | QM = %.4f\n", SQ_Res, gl_res, QM_Res))
cat(sprintf("Total:     SQ = %.4f | GL = %d\n\n", SQ_Tot, gl_tot))

# ANOVA direta no R:
cat("Tabela ANOVA via anova(modelo):\n")
tabela_anova <- anova(modelo)
print(tabela_anova)

# ==============================================================================
# e) Qual o coeficiente de determinacao R^2 desse estudo. Interprete.
# ==============================================================================

# R^2 = SQ_Reg / SQ_Tot = r^2
R2 <- SQ_Reg / SQ_Tot
cat("\n--- Item e: Coeficiente de Determinacao R^2 ---\n")
cat(sprintf("R^2 (calculado): %.4f (ou %.2f%%)\n", R2, R2 * 100))
cat(sprintf("R^2 (via summary): %.4f\n\n", summary(modelo)$r.squared))

# Interpretacao:
# Cerca de 97.56% da variacao total observada no IMC e explicada pelo consumo
# semanal de fast-food atraves do modelo linear ajustado. Os restantes 2.44%
# representam variabilidade residual (outros fatores nao medidos e erro amostral).

# ==============================================================================
# f) Construa o intervalo de 95% de confianca para os parametros (beta0 e beta1)
# ==============================================================================

# Fórmulas manuais:
# t_critico com alpha = 0.05 e gl = n - 2
alpha <- 0.05
t_crit <- qt(1 - alpha / 2, df = gl_res)

# Erros padroes:
# EP(beta1) = sqrt(QM_Res / Sxx)
# EP(beta0) = sqrt(QM_Res * (1/n + media_x^2 / Sxx))
ep_beta1 <- sqrt(QM_Res / Sxx)
ep_beta0 <- sqrt(QM_Res * (1 / n + (media_x^2) / Sxx))

ic_beta0 <- c(beta0_hat - t_crit * ep_beta0, beta0_hat + t_crit * ep_beta0)
ic_beta1 <- c(beta1_hat - t_crit * ep_beta1, beta1_hat + t_crit * ep_beta1)

cat("--- Item f: Intervalos de 95% de Confianca ---\n")
cat(sprintf("IC 95%% para beta0 (Intercepto): [%.4f, %.4f]\n", ic_beta0[1], ic_beta0[2]))
cat(sprintf("IC 95%% para beta1 (Inclinacao):  [%.4f, %.4f]\n\n", ic_beta1[1], ic_beta1[2]))

# Intervalos de confianca via confint() no R:
cat("Intervalos de confianca via confint(modelo, level = 0.95):\n")
print(confint(modelo, level = 0.95))

# ==============================================================================
# g) Qual seria o IMC previsto para um paciente para um consumo de 4? E qual o intervalo de confianca para esse valor?
# ==============================================================================

x0 <- 4

# Previsao pontual: y_hat = beta0 + beta1 * x0
imc_previsto <- beta0_hat + beta1_hat * x0

# Como a pergunta pede a estimativa para UM paciente individual ("um paciente"),
# utiliza-se o Intervalo de Predicao para uma nova observacao Y_novo:
# Var(Y_novo - y_hat) = QM_Res * (1 + 1/n + (x0 - media_x)^2 / Sxx)
# EP_pred = sqrt(QM_Res * (1 + 1/n + (x0 - media_x)^2 / Sxx))
ep_pred <- sqrt(QM_Res * (1 + 1 / n + ((x0 - media_x)^2) / Sxx))
ic_pred <- c(imc_previsto - t_crit * ep_pred, imc_previsto + t_crit * ep_pred)

cat("\n--- Item g: Previsao e Intervalo de Predicao para um Paciente (Consumo = 4) ---\n")
cat(sprintf("IMC previsto para o paciente: %.4f kg/m^2\n", imc_previsto))
cat(sprintf("Intervalo de 95%% de Predicao (paciente individual): [%.4f, %.4f]\n\n", ic_pred[1], ic_pred[2]))

# Utilizando a funcao predict() com interval = "prediction":
novo_paciente <- data.frame(Consumo = 4)
cat("Resultado via predict(interval = 'prediction'):\n")
pred_individual <- predict(modelo, newdata = novo_paciente, interval = "prediction", level = 0.95)
print(pred_individual)


# ==============================================================================
# h) O que se espera que aconteca com o IMC da pessoa que mais come no fast-food se ela comesse somente uma vez na semana?
# ==============================================================================

# Na base de dados, o consumo maximo observado e de 7 vezes por semana (Pacientes 1 e 19).
# x_atual = 7 e x_novo = 1.
# Variacao no consumo: Delta_X = 1 - 7 = -6 vezes/semana.
#
# Pelo modelo linear: Delta_Y = beta1 * Delta_X
consumo_max <- max(dados$Consumo)
consumo_novo <- 1
delta_consumo <- consumo_novo - consumo_max

variacao_imc <- beta1_hat * delta_consumo

cat("\n--- Item h: Reducao no IMC para o maior consumidor ---\n")
cat(sprintf("Consumo maximo atual: %d vezes/semana\n", consumo_max))
cat(sprintf("Novo consumo: %d vez/semana\n", consumo_novo))
cat(sprintf("Variacao no consumo (Delta X): %d vezes/semana\n", delta_consumo))
cat(sprintf("Variacao esperada no IMC: %.4f kg/m^2\n", variacao_imc))
cat(sprintf("Ou seja, espera-se uma REDUCAO de aproximadamente %.2f kg/m^2 no IMC.\n", abs(variacao_imc)))
