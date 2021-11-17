# Codigo do projeto

# Projeto Delivery de Comida - Projeto Clusters
#install.packages("rjson")
#install.packages("ggcorrplot")

library(tidyverse)
library(tidymodels)
library(skimr)
library(plyr)
library(dplyr)
library(torch)
library(keras)
library(magrittr)
library(ggplot2)
library(plotly)
library("rjson")
library(ggcorrplot)

# Objetivos: Criar uma analise cluster do data set abaixo, apontando quantos grupos (n* ótimo) existe na cidade do data set analisado,
# Descrever as principais características de cada grupo, realizar analise visual dos grupos, identificar melhorias para o app

## Data Set -----

df <- read.csv('onlinedeliverydata.csv')

bgl <- sf::read_sf("C:/Users/USER/Desktop/Pos Data Science/Projetos Legais/Rappi/PincodeBoundary-master/Bangalore/boundary.geojson")



# Observações do Data set, não tem data faltante e são poucos dados, maioria das variaveis são categóricas

## Ajuste de dados-----

df %>% glimpse()

## Evitar usar recipes para arrumar as bases, Usar as bibliotecas do

# Receita <- recipe(Output ~ ., data = df) %>%
#   step_select(-Reviews) %>%
#   step_normalize(all_numeric()) %>%
#   step_dummy(all_nominal()) %>%
#   step_bin2factor(Output_Yes) %>%
#   step_nzv(all_numeric_predictors()) #posso tirar all_numerics e deixar vazia?

Receita <- recipe(Output ~ ., data = df) %>%
  step_select(-Reviews) %>%
  step_dummy(all_nominal()) %>%
  step_nzv(all_numeric_predictors())

preparado <- prep(Receita, df)
df2 <- bake(preparado, new_data = NULL)

# dfteste <- df %>%
#   select(-Review) %>%
#   model.matrix(Output~., data = .) %>%
#   as.data.frame()

df %>% count("Maximum.wait.time")


glimpse(df2)

## Criando visualizações para entender a base -------

# Continuas: Idade e Tamanho família

# df %>%
#   dplyr::count(Age, Output) %>%
#   ggplot(aes(Age, n))+
#   geom_col(aes(fill = Output), position="dodge")
#
# (df %>%
#    ungroup() %>%
#     #group_by(Age) %>%
#     count(Age, Output) %>%
#     ggplot(aes(Age, n))+
#     geom_bar(fill = x))

df %>%
  ggplot(aes(Age, fill = Output)) +
  geom_bar(position = 'dodge')+
  scale_x_continuous(breaks = 10:50)

df2 %>%
  ggplot(aes(Family.size, fill = factor(Output_Yes))) +
  geom_bar(position = 'dodge')

# Variáveis Discretas

df %>%
  ggplot(aes(Monthly.Income, fill = factor(Output))) +
  geom_bar(position = 'dodge')+
  #scale_fill_brewer(palette = 'RdBu')+
  theme_classic()

colunas <- df %>%
  select(-latitude, -longitude, - Reviews, -Output, -Pin.code) %>%
  colnames()
graficos <- list()

for (x in colunas){
  graficos[[x]] <- df %>%
    ggplot(aes(.data[[x]], fill = Output)) +
    geom_bar(position = 'dodge', show.legend = FALSE)
}

faz_grafico <- function(x) {
  df %>%
    ggplot(aes(.data[[x]], fill = Output)) +
    geom_bar(position = 'dodge', show.legend = FALSE)
}
graficos <- map(colunas, faz_grafico)

patchwork::wrap_plots(graficos)



graficos$Medium..P1.
## Correlação ----

# Precisa primeiro converter o texto em uma ordem q a maquina entenda:

# df[df=="Strongly disagree"] <- -3
# df[df=="Disagree"] <- -1
# df[df=="Agree"] <- 1
# df[df=="Strongly Agree"] <- 3
# df[df=="Neutral"] <- 0


valores <- c('Agree', "Strongly Agree", "Neutral", "Disagree","Strongly disagree")

colunas_likert <- df %>% select(where(~any(.x %in% valores))) %>% colnames()



df <- df %>% 
  mutate(across(c(colunas_likert),~revalue(.x, c("Strongly disagree"= -3,
                               "Disagree"= -1,
                               "Agree"= 1,
                               "Strongly agree"= 3,
                               "Strongly Agree"= 3,
                               "Neutral"= 0)))) %>%
  mutate(across(c(colunas_likert), ~strtoi(.x))) %>% 
  mutate(Output = ifelse(Output == "Yes", 1, 0)) %>% 
  glimpse()




df %>% mutate("More.restaurant.choices" = revalue(c("Strongly disagree"= -3,
                                                    "Disagree"= -1,
                                                    "Agree"= 1,
                                                    "Strongly agree"= 3,
                                                    "Neutral"= 0)))

for (x in colunas_likert){
  df %>% revalue(x, c("Strongly disagree"= -3,
                                   "Disagree"= -1,
                                   "Agree"= 1,
                                   "Strongly agree"= 3,
                                   "Neutral"= 0))
  
  
}

likert <- function(x) {
  revalue(df$x, c("Strongly disagree"= -3,
                  "Disagree"= -1,
                  "Agree"= 1,
                  "Strongly agree"= 3,
                  "Neutral"= 0))
}

dfcor <- map(colunas_likert, likert)

df %>% glimpse()

correlacao <- cor(df %>% select(where(is.numeric)))

likert

ggcorrplot(correlacao, hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))

## Kmeans -----

tibble(k = 2:50) %>%
  mutate(w = map_dbl(k, ~kmeans(df2, centers = .x)$tot.withinss)) %>%
  ggplot(aes(k, w)) +
  geom_point() +
  geom_line()

# Duvida, como ver o numero ótimo de centers (k) no meu gráfico? (no exemplo coloquei 4 para facilitar a visualização)
k_medias <- kmeans(df2, centers = 11)

k_medias$totss

df_graf <- df %>%
  mutate(cluster = k_medias$cluster) #pq usaram factor(k_medias$cluster)

df_mean <-  df2 %>%
  mutate(cluster = k_medias$cluster)

df_graf %>% glimpse()

## Distrivuição no gráfico de pontos ----

df_graf%>%
  ggplot() +
  geom_point(
    aes(x = longitude,
        y = latitude,
        colour = factor(cluster)))


gg_pontos_com_mapa <- bgl %>%
  ggplot() +
  geom_sf(colour = "black", size = .1) +
  geom_point(
    aes(x = longitude,
        y = latitude,
        colour = factor(cluster)),
    data = df_graf,
    alpha = .7
  )


gg_pontos_com_mapa


plot_ly(x = df_graf$Age,
        y = df_graf$Family.size,
        z = df_graf$Monthly.Income,
        # position = "jitter",
        type = "scatter3d", mode = "markers", size = 2,
        color = factor(df_graf$cluster),
        colors = RColorBrewer::brewer.pal(5, "Dark2")) %>%
  layout(
    scene = list(
      xaxis = list(title = "Idade"),
      yaxis = list(title = "Tamanho da família"),
      zaxis = list(title = "Renda familiar")
    ))
## Perfil dos clusters: ----
df_mean %>%
  group_by(cluster) %>%
  summarise(Age = mean(Age),
            Family_size = mean(Family.size),
            Porcentagem_Homem = mean(Gender_Male), #Tirar pq pouco relevante
            Solteiros = mean(Marital.Status_Single),
            Estudante = mean(Occupation_Student),
            Renda_baixa = mean(Monthly.Income_Below.Rs.10000) + mean(Monthly.Income_No.Income),
            Resposta = mean(Output_Yes)
  )

## Oportunidade de Negócio:

df_mean %>%
  group_by(cluster) %>%
  summarise(Age = mean(Age),
            Family_size = mean(Family.size),
            Porcentagem_Homem = mean(Gender_Male), #Tirar pq pouco relevante
            Solteiros = mean(Marital.Status_Single),
            Estudante = mean(Occupation_Student),
            Renda_baixa = mean(Monthly.Income_Below.Rs.10000) + mean(Monthly.Income_No.Income),
            Resposta = mean(Output_Yes)
  )


##Tidymodel reg log -----


Receita2 <- recipe(Output ~ ., data = df) %>%
  step_select(-Reviews, -Pin.code) %>%
  step_normalize(all_numeric()) %>%
  step_dummy(all_nominal()) %>%
  #step_bin2factor(Output_Yes) %>%
  step_nzv(all_numeric_predictors())

preparado <- prep(Receita2, df)
assado <- bake(preparado, new_data = NULL)

split <- initial_split(assado)
train <- training(split)
test  <- testing(split)

fit_glm <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification") %>%
  fit(Output_Yes ~ ., train)

fitted <- fit_glm %>%
  predict(new_data = test, type = "prob") %>%
  mutate(observado = test$Output_Yes,
         modelo = "logística")

head(fitted)

typeof(fitted)

fitted$obs2 <- ifelse(fitted$observado == 'yes', 1, 0)

roc <- fitted %>%
  roc_auc(observado, .pred_yes)

rmse <- fitted %>%
  rmse(obs2, .pred_yes)

fim <- rmse %>% bind_rows(roc)

fim

## LASSO

lasso <- logistic_reg(penalty = tune(), mixture = 1) %>% # define o modelo lasso e o parametro a ser tunado (o lambda)
  set_engine("glmnet") %>%
  set_mode("classification")

lasso

# validaÃ§Ã£o cruzada para ajuste do hiperparametro em 10 lotes
cv_split <- vfold_cv(train, v = 10, strata = "Output_Yes")

lambda_tune <- tune_grid(lasso, # especificacao do modelo
                         Receita2,# a receita a ser aplicada a cada lote
                         resamples = cv_split, # os lotes da validacao cruzada
                         grid = 50,# quantas combinacoes de parametros vamos considerar
                         metrics = metric_set(roc_auc, accuracy)) # metricas consideradas

autoplot(lambda_tune)

lambda_tune %>%
  collect_metrics()

best <- lambda_tune %>%
  select_best("roc_auc")

#head(fitted %>% mutate(prob_y = (fitted$.pred_yes)) %>% mutate(predito = if .pred_yes >=  ))

## Redes neurais
folds <- vfold_cv(train, v = 5)

mod <- mlp(
  hidden_units = tune(),
  epochs = 10
) %>%
  set_engine("keras") %>%
  set_mode("classification")

fit_lmp <- mod %>%
  fit(Output_Yes ~ ., train)
