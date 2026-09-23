# 2. Conjunto de Dados do Exercício 2
horas_treinamento <- c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 6, 8, 10, 12)
tempo_tarefa <- c(24, 26, 23, 19, 21, 18, 17, 16, 14, 13, 11, 10, 22, 18, 20, 17)
dados <- data.frame(HorasDeTreinamento = horas_treinamento, TempoParaTarefa = tempo_tarefa)

n <- nrow(dados)

# ==============================================================================
# a) Coeficiente de correlacao linear de Pearson e teste de significancia
# ==============================================================================

Sxx <- sum((dados$HorasDeTreinamento - mean(dados$HorasDeTreinamento))^2)
Syy <- sum((dados$TempoParaTarefa - mean(dados$TempoParaTarefa))^2)
Sxy <- sum((dados$HorasDeTreinamento - mean(dados$HorasDeTreinamento)) * (dados$TempoParaTarefa - mean(dados$TempoParaTarefa)))

cat("--- Item a: Somas de Quadrados e Correlacao ---\n")
cat("Sxx:", Sxx, "\n")
cat("Syy:", Syy, "\n")
cat("Sxy:", Sxy, "\n")

r <- Sxy / sqrt(Sxx * Syy)
cat("r (manual):", r, "\n\n")

teste_cor <- cor.test(dados$HorasDeTreinamento, dados$TempoParaTarefa, method = "pearson")
print(teste_cor)

# ==============================================================================
# b) Encontre a equacao da reta de regressao
# ==============================================================================

# Medias amostrais:
media_x <- mean(dados$HorasDeTreinamento)
media_y <- mean(dados$TempoParaTarefa)

# Estimadores de MQO (Minimos Quadrados Ordinarios):
# beta1_hat = Sxy / Sxx
# beta0_hat = media_y - beta1_hat * media_x
beta1_hat <- Sxy / Sxx
beta0_hat <- media_y - beta1_hat * media_x

cat("\n--- Item b: Coeficientes da Reta de Regressao ---\n")
cat("Media de X (Horas):", media_x, "\n")
cat("Media de Y (Tempo):", media_y, "\n")
cat("beta1 (inclinacao):", beta1_hat, "\n")
cat("beta0 (intercepto):", beta0_hat, "\n")
cat(sprintf("Equacao da reta: Tempo_chapeu = %.4f + (%.4f) * Horas\n", beta0_hat, beta1_hat))
cat(sprintf("Ou simplesmente: Tempo_chapeu = %.4f - %.4f * Horas\n\n", beta0_hat, abs(beta1_hat)))

# Ajuste via funcao lm() do R:
modelo <- lm(TempoParaTarefa ~ HorasDeTreinamento, data = dados)
cat("Resumo do modelo ajustado via lm():\n")
print(summary(modelo))

# ==============================================================================
# c) Interprete os coeficientes beta0 e beta1 no contexto do problema
# ==============================================================================

# Interpretacao de beta0 (Intercepto = 26.1625):
# Representa o tempo medio esperado para completar a tarefa quando o funcionario
# nao possui nenhum treinamento prévio (HorasDeTreinamento = 0).
# No contexto: um trabalhador com 0 horas de treinamento levaria, em media,
# aproximadamente 26.16 unidades de tempo para concluir a tarefa na linha de producao.
# Nota: Embora 0 esteja ligeiramente fora do intervalo observado [2, 24], faz sentido
# pratico no contexto de treinamento de novos funcionarios.

# Interpretacao de beta1 (Inclinacao = -0.6750):
# Representa a taxa media de variacao no tempo de conclusao para cada hora adicional de treino.
# No contexto: para cada 1 hora a mais de treinamento que o funcionario recebe,
# espera-se uma REDUCAO media de 0.675 unidades de tempo no cumprimento da tarefa.
# O sinal negativo reflete o ganho de produtividade / agilidade decorrente do treinamento.

# ==============================================================================
# d) Construa a ANOVA e verifique se a regressao pode ser considerada significativa
# ==============================================================================

# Calculos manuais das Somas de Quadrados:
# SQ_Reg = beta1_hat * Sxy = (-0.675) * (-432) = 291.6
# SQ_Tot = Syy = 314.9375
# SQ_Res = SQ_Tot - SQ_Reg = 23.3375
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

# R^2 = SQ_Reg / SQ_Tot = r^2
R2 <- SQ_Reg / SQ_Tot
R2_summary <- summary(modelo)$r.squared

cat("\n--- Item e: Coeficiente de Determinacao R^2 ---\n")
cat(sprintf("R^2 (calculado): %.4f (ou %.2f%%)\n", R2, R2 * 100))
cat(sprintf("R^2 (via summary): %.4f\n\n", R2_summary))

# Interpretacao:
# Cerca de 92.59% da variacao total observada no tempo de conclusao da tarefa
# e explicada pelo numero de horas de treinamento recebidas, segundo o modelo de
# regressao linear ajustado. Os restantes 7.41% representam variabilidade nao
# explicada pelo modelo (diferencas individuais de habilidade, cansaco, erro amostral).

# ==============================================================================
# f) Construa o intervalo de 95% de confianca para os parametros (beta0 e beta1)
# ==============================================================================

# Formulas manuais:
# Valor critico t com alpha = 0.05 e gl = n - 2 = 14
alpha <- 0.05
t_crit <- qt(1 - alpha / 2, df = gl_res)

# Erros padroes dos estimadores:
# EP(beta1) = sqrt(QM_Res / Sxx)
# EP(beta0) = sqrt(QM_Res * (1/n + media_x^2 / Sxx))
ep_beta1 <- sqrt(QM_Res / Sxx)
ep_beta0 <- sqrt(QM_Res * (1 / n + (media_x^2) / Sxx))

# Intervalos: estimador +- t_crit * EP
ic_beta0 <- c(beta0_hat - t_crit * ep_beta0, beta0_hat + t_crit * ep_beta0)
ic_beta1 <- c(beta1_hat - t_crit * ep_beta1, beta1_hat + t_crit * ep_beta1)

cat("--- Item f: Intervalos de 95% de Confianca ---\n")
cat(sprintf("Valor critico t (gl = %d, alpha = 0.05): %.4f\n", gl_res, t_crit))
cat(sprintf("IC 95%% para beta0 (Intercepto): [%.4f, %.4f]\n", ic_beta0[1], ic_beta0[2]))
cat(sprintf("IC 95%% para beta1 (Inclinacao):  [%.4f, %.4f]\n\n", ic_beta1[1], ic_beta1[2]))

# Intervalos de confianca via funcao confint() do R:
cat("Intervalos via confint(modelo, level = 0.95):\n")
print(confint(modelo, level = 0.95))

# ==============================================================================
# g) Qual seria o tempo previsto para um funcionario com 7 horas de treinamento?
# ==============================================================================

x0 <- 7

# Previsao pontual: y_hat = beta0 + beta1 * x0
tempo_previsto <- beta0_hat + beta1_hat * x0

# Erro padrao para predicao individual de UM funcionario (Y_novo):
# EP_pred = sqrt(QM_Res * (1 + 1/n + (x0 - media_x)^2 / Sxx))
ep_pred <- sqrt(QM_Res * (1 + 1 / n + ((x0 - media_x)^2) / Sxx))
ic_pred <- c(tempo_previsto - t_crit * ep_pred, tempo_previsto + t_crit * ep_pred)

# Erro padrao para a media esperada E(Y|X0 = 7):
ep_media <- sqrt(QM_Res * (1 / n + ((x0 - media_x)^2) / Sxx))
ic_media <- c(tempo_previsto - t_crit * ep_media, tempo_previsto + t_crit * ep_media)

cat("\n--- Item g: Previsao de Tempo para 7 Horas de Treinamento ---\n")
cat(sprintf("Tempo previsto pontual (X = 7): %.4f unidades de tempo\n", tempo_previsto))
cat(sprintf("Intervalo de 95%% de Predicao (para UM funcionario): [%.4f, %.4f]\n", ic_pred[1], ic_pred[2]))
cat(sprintf("Intervalo de 95%% de Confianca (para a MEDIA dos funcionarios): [%.4f, %.4f]\n\n", ic_media[1], ic_media[2]))

# Utilizando a funcao predict() do R:
novo_funcionario <- data.frame(HorasDeTreinamento = 7)
cat("Via predict(interval = 'prediction'):\n")
print(predict(modelo, newdata = novo_funcionario, interval = "prediction", level = 0.95))

# ==============================================================================
# h) Se um funcionario aumentar seu treinamento em 3 horas, qual a reducao esperada no tempo de tarefa?
# ==============================================================================

# O efeito marginal por hora adicional de treinamento e dado pela inclinacao beta1:
# Delta_Y = beta1 * Delta_X
delta_horas <- 3
variacao_tempo <- beta1_hat * delta_horas
reducao_esperada <- abs(variacao_tempo)

cat("\n--- Item h: Reducao no Tempo por 3 Horas Adicionais de Treino ---\n")
cat(sprintf("Aumento no treinamento (Delta X): %d horas\n", delta_horas))
cat(sprintf("Variacao calculada no tempo (Delta Y): %.4f unidades de tempo\n", variacao_tempo))
cat(sprintf("Reducao esperada no tempo de tarefa: %.4f unidades de tempo\n", reducao_esperada))
cat(sprintf("Portanto, espera-se uma reducao de aproximadamente %.2f unidades de tempo.\n", reducao_esperada))
