lambda_dia = 3
lambda_2dias = lambda_dia * 2
lambda_12h = lambda_dia / 2

acidentes_a = 2
prob_a = dpois(acidentes_a, lambda_dia)

acidentes_b = 0
prob_b = 1 - dpois(acidentes_b, lambda_dia)

acidentes_c = 4
prob_c = ppois(acidentes_c, lambda_dia)

acidentes_d = 5
prob_d = dpois(acidentes_d, lambda_2dias)

acidentes_e = 0
prob_e = dpois(acidentes_e, lambda_12h)

prob_a
prob_b
prob_c
prob_d
prob_e
