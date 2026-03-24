lancamentos = 12
prob_uniforme = 1/6

sucessos_a = 2
prob_a = dbinom(sucessos_a, lancamentos, prob_uniforme)

sucessos_b = 1
prob_b = pbinom(sucessos_b, lancamentos, prob_uniforme)

sucessos_c = 3
prob_c = 1 - pbinom(sucessos_c, lancamentos, prob_uniforme)

sucessos_d1 = 1
sucessos_d2 = 5
dist_d1 = pbinom(sucessos_d1, lancamentos, prob_uniforme)
dist_d2 = pbinom(sucessos_d2, lancamentos, prob_uniforme)
prob_d = dist_d2 - dist_d1

esperanca = lancamentos * prob_uniforme
variancia = lancamentos * prob_uniforme * (1 - prob_uniforme)

prob_a
prob_b
prob_c
prob_d
esperanca
variancia
