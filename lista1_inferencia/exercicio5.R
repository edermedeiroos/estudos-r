media_atendimento = 5
taxa = 1 / media_atendimento

prob_a = pexp(3, rate = taxa)

prob_b = 1 - pexp(10, rate = taxa)

prob_c = pexp(8, rate = taxa) - pexp(4, rate = taxa)

prob_d = 0.20
tempo_d = qexp(prob_d, rate = taxa)

prob_a
prob_b
prob_c
tempo_d
