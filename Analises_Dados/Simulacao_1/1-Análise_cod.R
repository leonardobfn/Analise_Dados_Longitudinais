# Pacotes-------
library(ggplot2)
library(dplyr)

# Dados do exemplo transversal (cross-section) --------
df <- data.frame(
  pessoa = LETTERS[1:8],
  idade  = c(27,27,27,27, 28,28,28,28),
  sexo   = c(0,0,1,1, 0,0,1,1) |> as.factor(), # 0 = mulher, 1 = homem
  PA     = c(112,114,118,117, 116,118,122,121)
)

df
# Médias por idade (todas as pessoas)
tapply(df$PA, df$idade, mean)

# Médias por sexo a idade = 27
subset27 <- subset(df, idade == 27)
tapply(subset27$PA, subset27$sexo, mean)

# Médias por idade e sexo
mean_cross <- df %>%
  group_by(idade, sexo) %>%
  summarise(PA_media = mean(PA)) 
mean_cross

model = lm(PA ~  idade, data = df)
summary(model)
# Gráfico 1: Estudo transversal (médias por idade e sexo)
ggplot(df, aes(x = factor(idade), y = PA, fill = factor(sexo))) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +  # sem outliers duplicados
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "yellow") +
  labs(x = "idade", y = "Pressão Arterial (mmHg)",
       fill = "Sexo",
       title = "Distribuição da Pressão Arterial por Sexo") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "tomato"),
                    labels = c("Mulher","Homem")) +
  theme_minimal()


# Simulação sob normalidade para aula (considerando mais observações):
set.seed(123)
n  <- 200
sexo_sim  <- rbinom(n, 1, 0.5) 
idade_sim <- sample(20:60, n, replace = TRUE)
# Verdade do gerador: intercepto 90, +0.5 mmHg/ano, +50 mmHg se homem, erro ~ N(0, 8^2)
PA_sim <- 90 + 0.5*idade_sim + 50*sexo_sim + rnorm(n, 0, 8)
sim <- data.frame(PA = PA_sim, idade = idade_sim, sexo = sexo_sim)

m_sim <- lm(PA ~ idade + as.factor(sexo_sim), data = sim)
summary(m_sim)

ggplot(sim, aes(x = idade, y = PA, color = factor(sexo_sim))) +
  geom_point(alpha = 0.6) +                  # pontos
  geom_smooth(method = "lm", se = FALSE) +   # reta linear por grupo
  labs(x = "Idade (anos)", y = "Pressão Arterial (mmHg)", color = "Sexo") +
  theme_minimal()




# Exemplo longitudinal simulado (trajetórias individuais) -------
set.seed(123)
n_ind <- 20
idades <- 27:30
df_long <- data.frame()

sexo_ind <- c(rep(c(0,1),each=n_ind/2))

for(i in 1:n_ind){
  base  <- rnorm(1, mean = 110, sd = 5)
  slope <- rnorm(1, mean = 1.2, sd = 0.3)
  noise <- rnorm(length(idades), mean = 0, sd = 2)
  PA_traj <- base + slope * (idades - 27) + 1*sexo_ind[i]+noise
  df_long <- rbind(df_long,
                   data.frame(ind = i, idade = idades, PA = PA_traj,
                              sexo = sexo_ind[i]))
}

# Gráfico 2: Exemplo longitudinal (trajetórias)
ggplot(df_long, aes(x = idade, y = PA, group = ind, color = factor(sexo))) +
  geom_line() + geom_point() +
  labs(x = "Idade (anos)", y = "Pressão Arterial (mmHg)",
       title = "Exemplo Longitudinal (trajetórias individuais)",
       color = "Indivíduo") +
  theme_minimal()
