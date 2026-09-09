# Estudos em R

Repositório dedicado ao estudo e prática da linguagem R, com foco em probabilidade, distribuições estatísticas e inferência.

---

## Estrutura do Repositório

```text
estudos-r/
├── lista1_inferencia/
│   ├── exercicio1.R   # Distribuição Normal Padrão N(0, 1) e quantis
│   ├── exercicio2.R   # Distribuição Normal N(550, 100) aplicada
│   ├── exercicio3.R   # Distribuição Binomial (ensaios de Bernoulli, esperança e variância)
│   ├── exercicio4.R   # Distribuição de Poisson (taxas por intervalo de tempo)
│   ├── exercicio5.R   # Distribuição Exponencial (tempo de atendimento)
│   └── exercicio6.R   # Visualização gráfica das distribuições (Binomial, Normal e Poisson)
├── R.Rproj            # Projeto RStudio
└── README.md          # Documentação do projeto
```

---

## Conteúdo

### Lista 1: Inferência e Distribuições de Probabilidade (`lista1_inferencia/`)

| Script | Tema | Funções R Utilizadas |
| :--- | :--- | :--- |
| [`exercicio1.R`](file:///c:/Users/Eder/Repositorys/estudos-r/lista1_inferencia/exercicio1.R) | Cálculo de probabilidades e quantis na **Distribuição Normal Padrão** $Z \sim N(0, 1)$. | `pnorm()`, `qnorm()` |
| [`exercicio2.R`](file:///c:/Users/Eder/Repositorys/estudos-r/lista1_inferencia/exercicio2.R) | Aplicação prática da **Distribuição Normal** com média $\mu = 550$ e desvio padrão $\sigma = 100$. | `pnorm()`, `qnorm()` |
| [`exercicio3.R`](file:///c:/Users/Eder/Repositorys/estudos-r/lista1_inferencia/exercicio3.R) | Análise de **Distribuição Binomial** $B(n, p)$, probabilidades acumuladas/pontuais, média e variância. | `dbinom()`, `pbinom()` |
| [`exercicio4.R`](file:///c:/Users/Eder/Repositorys/estudos-r/lista1_inferencia/exercicio4.R) | Modelagem de eventos raros via **Distribuição de Poisson** com ajuste do parâmetro $\lambda$ para diferentes períodos. | `dpois()`, `ppois()` |
| [`exercicio5.R`](file:///c:/Users/Eder/Repositorys/estudos-r/lista1_inferencia/exercicio5.R) | Modelagem de intervalos de tempo contínuos com **Distribuição Exponencial** $\text{Exp}(\lambda)$. | `pexp()`, `qexp()` |
| [`exercicio6.R`](file:///c:/Users/Eder/Repositorys/estudos-r/lista1_inferencia/exercicio6.R) | Geração de gráficos comparativos para curvas e probabilidades (Binomial, Normal e Poisson) com legendas. | `plot()`, `lines()`, `legend()` |

---

## Pré-requisitos

Para executar os scripts, recomenda-se ter instalado:

- [R (>= 4.0.0)](https://cran.r-project.org/)
- [RStudio Desktop](https://posit.co/download/rstudio-desktop/) (opcional, mas recomendado)

Os exercícios utilizam pacotes nativos do R (`stats` e `graphics`), não necessitando da instalação de bibliotecas externas adicionais.

---

## Como Executar

Você pode executar os scripts diretamente pelo terminal com `Rscript` ou dentro do ambiente R/RStudio.

### Via Linha de Comando:
```bash
# Executando um exercício específico
Rscript lista1_inferencia/exercicio1.R
```

### No Console R:
```R
source("lista1_inferencia/exercicio1.R")
```

