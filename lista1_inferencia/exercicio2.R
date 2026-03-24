media = 550
desvio_padrao = 100

ponto_a = 700
prob_a = 1 - pnorm(ponto_a, media, desvio_padrao)

ponto_b1 = 450
ponto_b2 = 650
dist_b1 = pnorm(ponto_b1, media, desvio_padrao)
dist_b2 = pnorm(ponto_b2, media, desvio_padrao)
prob_b = dist_b2 - dist_b1

prob_c = 0.10
ponto_c = qnorm(1-prob_c, media, desvio_padrao)

prob_d = 0.15
ponto_d = qnorm(prob_d, media, desvio_padrao)

prob_a
prob_b
ponto_c
ponto_d
