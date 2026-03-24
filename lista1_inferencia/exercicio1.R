media = 0
desvio_padrao = 1

ponto_a = 1.96
prob_a = pnorm(ponto_a, media, desvio_padrao)

ponto_b = -1.5
prob_b = 1 - pnorm(ponto_b, media, desvio_padrao)

ponto_c1 = -1.64
ponto_c2 = 1.64
prob_c = 1 - 2*pnorm(ponto_c1, media, desvio_padrao)

prob_d = 0.95
ponto_d = qnorm(prob_d, media, desvio_padrao)

prob_e = 0.025
ponto_e = qnorm(1-prob_e, media, desvio_padrao)

prob_a
prob_b
prob_c
ponto_d
ponto_e
