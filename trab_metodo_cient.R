library(tidyverse)
library(ggplot2)
library(lawstat)
library(xtable)

dados <- read_csv(file.choose())

experimentos <- dados %>% group_by(id) %>% mutate(pontuacao = sum(pontuacao)) %>% ungroup() %>%
  filter(id_teste == 1) %>% select(-c(2,3,4)) %>% mutate(personalidade = ifelse(personalidade %in% c("pouco-introvertido","introvertido"), "introvertido", "extrovertido"))

modelo <- aov(pontuacao ~ musica, experimentos)

ggplot(experimentos, aes(x = musica, y = pontuacao, fill = musica))+
  geom_violin(color = F)+
  geom_hline(aes(yintercept = mean(pontuacao), linetype = "Pontuação Média"), color = "darkred")+
  scale_x_discrete(labels = NULL, breaks = NULL)+
  scale_fill_discrete(name = "Tratamento", labels = c("Controle", "Grieg", "Tchaikovsky","Schumann","Strauss"))+
  scale_linetype_discrete(name = "")+
  labs(x = "", y = "Pontuação")+
  theme_minimal()

levene.test(experimentos$pontuacao,experimentos$musica)

ggplot(modelo, aes(sample = rstandard(modelo)))+
  geom_qq(color = 4)+
  geom_qq_line(color = "darkred")+
  labs(x = "Quantil teórico",y = "Quantil empírico")+
  theme_minimal()

ggplot(modelo, aes(x = .fitted, y = rstandard(modelo)))+
  geom_point( color = 4)+
  geom_hline(yintercept = 0, linetype = "longdash", color = "darkred")+
  labs(x = "Ajustado",y = "Resíduo Padronizado")+
  theme_minimal()

ggplot(modelo, aes(x = 1:15,y = rstandard(modelo)))+
  geom_point( color = 4)+
  geom_hline(yintercept = 0, linetype = "longdash", color = "darkred")+
  labs(x = "Ordem de Experimentação",y = "Resíduo Padronizado")+
  theme_minimal()

ggplot()+
  geom_point(aes(x = modelo$model$musica,y = rstandard(modelo)), color = 4)+
  geom_hline(yintercept = 0, linetype = "longdash", color = "darkred")+
  scale_x_discrete(labels = c("Controle","Grieg","Tchaikovsky","Schumann", "Strauss"))+
  labs(x = "", y = "Resíduo Padronizado")+
  theme_minimal()

shapiro.test(modelo$residuals)


ggplot(experimentos, aes(x = musica, y = pontuacao, fill = musica)) +
  geom_boxplot(color = "black") +
  scale_fill_discrete(name = "Tratamento", labels = c("Controle", "Grieg", "Tchaikovsky","Schumann","Strauss")) +
  labs(x = "", y = "Pontuação") +
  theme_minimal()

summary_stats <- experimentos %>%
  group_by(musica) %>%
  summarise(
    Media = mean(pontuacao),
    Desvio_Padrao = sd(pontuacao),
    Minimo = min(pontuacao),
    Maximo = max(pontuacao),
    Q1 = quantile(pontuacao, 0.25),
    Mediana = median(pontuacao),
    Q3 = quantile(pontuacao, 0.75)
  )
print(summary_stats)

