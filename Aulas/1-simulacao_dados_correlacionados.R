library(MASS)
library(dplyr)

set.seed(123)

# Função para simular e rodar os testes
sim_ttest <- function(n = 30, delta = 0, rho = 0.7,
                      nsim = 2000, alpha = 0.05) {
  
  # Matriz de covariância para (before, after)
  Sigma <- matrix(c(1, rho, rho, 1), ncol = 2)
  
  rej_paired <- numeric(nsim)
  rej_ind <- numeric(nsim)
  
  for (i in seq_len(nsim)) {
    mu <- c(0, delta)   # diferença média entre before e after
    dados <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)
    
    before <- dados[,1]
    after  <- dados[,2]
    
    # Teste pareado (correto)
    p_paired <- t.test(after, before, paired = TRUE)$p.value
    # Teste independente (incorreto)
    p_ind <- t.test(after, before, paired = FALSE, var.equal = TRUE)$p.value
    
    rej_paired[i] <- (p_paired < alpha)
    rej_ind[i] <- (p_ind < alpha)
  }
  
  tibble(
    delta = delta,
    rej_paired = mean(rej_paired),
    rej_ind = mean(rej_ind)
  )
}

# Cenário 1: sem efeito (delta = 0) 

# Teste pareado: teste controlado, ou seja, um teste tem controle adequado do erro tipo I quando, sob h0
# a taxa real de rejeição é aproximadamente igual a alpha. 

# Teste independente: Pode subestimar ou superestimar o erro tipo I, dependendo da correlação

res_null <- sim_ttest(delta = 0,rho = .60,alpha = .10)

# Cenário 2: com efeito (delta = 0.5) ->  maior chance de detectar a diferença real

#Teste pareado: Explora a correlação dentro do indivíduo, reduz a variabilidade do efeito estimado
# maior chance de detectar a diferença real

#Teste independente: Ignora a correlação.
#Variabilidade maior → mais difícil detectar o efeito real.

res_effect <- sim_ttest(delta = .5,rho = .75)

bind_rows(res_null, res_effect)
