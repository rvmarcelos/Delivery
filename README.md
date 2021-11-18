# "Delivery"
## "Marcelo Setubal"


```{r setup, include=FALSE}
library(tidyverse)
library(tidymodels)
library(skimr)
library(plyr)
library(dplyr)
#library(torch) Torch está com problema
#library(keras)
library(magrittr)
library(ggplot2)
library(plotly)
library("rjson")
library(ggcorrplot)

df_raw <- read.csv('onlinedeliverydata.csv')

bgl <- sf::read_sf("C:/Users/USER/Desktop/Pos Data Science/Projetos Legais/Rappi/PincodeBoundary-master/Bangalore/boundary.geojson")

likert_agree <- c('Agree', "Strongly Agree", "Neutral", "Disagree","Strongly disagree")

likert_important <- c('Important', "Moderately Important", "Slightly Important", "Unimportant","Very Important")


colunas_likert_agree <- df_raw %>% select(where(~any(.x %in% likert_agree))) %>% colnames()

colunas_likert_important <- df_raw %>% select(where(~any(.x %in% likert_important))) %>% colnames()


df <- df_raw %>% 
  # Tratando variavel likert: agree to disagree
  mutate(across(colunas_likert_agree,~revalue(.x, c("Strongly disagree"= -3,
                               "Disagree"= -1,
                               "Agree"= 1,
                               "Strongly agree"= 3,
                               "Strongly Agree"= 3,
                               "Neutral"= 0)))) %>%
  mutate(across(c(colunas_likert_agree), ~strtoi(.x))) %>%
  # Tratando variavel likert unimportant to very important
  mutate(across(colunas_likert_important,
                ~revalue(.x,
                         c("Unimportant"= 0,
                           "Slightly Important"= 1,
                           "Moderately Important"= 2,
                           "Important"= 3,
                           "Very Important"= 4)))) %>%
  mutate(across(c(colunas_likert_important), ~strtoi(.x))) %>%
  # Tratando dummy da variavel resposta 
  mutate(Output_grafico = Output) %>% 
  mutate(Output = ifelse(Output == "Yes", 1, 0))  



```

## Desafio Kaggle: Online Food Delivery Preferences-Bangalore region

Esse desafio foi proposto pelo usuário Ben Roshan que disponibilizou dados de uma pesquisa de satisfação relacionada a delivery de comida na cidade de Bangalore na Índia. No desafio, Bren propoem aos usuários do Kaggle a criar um modelo de classificalção que diga a probabilidade do usuário pedir comida online novamente.

Além desse desafio, me propus a realizar uma analise de cluster usando K médias para identificar quais grupos diversos de consumidores de deliverys tem na cidade e quais seus costumes, de forma a identificar oportunidades.

[link do projeto](https://www.kaggle.com/benroshan/online-food-delivery-preferencesbangalore-region)

## Contexto do mercado de delivery

"The on-demand industry is brutally competitive, characterized by low customer loyalty and high cash burns from significant promotional spend."

the greatest route density in the world
lower labor costs than in the West
a long growth runway driven by low rates of online adoption.

-  Falar dos cases de sucesso vistos na america latina


[referência](https://sacra.com/research/rappi-the-meituan-of-latin-america/)
[referêcnia](https://www.prnewswire.com/news-releases/-44-23-billion-growth-in-global-online-on-demand-food-delivery-services-market-2020-2024--convenience-involved-in-ordering-food-online-to-be-key-driver--technavio-301246314.html)


## O Data Set

Com o aumento de demanda em Bangalore na Índia, foi realizada nesta cidade uma pesquisa de marketing com o objetivo de analisar o mercado potencia que resultou no data set utilizado neste desafio.

Por questão de estratégia da empresa que contratou a pesquisa, esta tem um foco em qualitativo dos dados, realizando diversas perguntas ao respondente, porém, isto limitou o número de respondentes. Dessa forma, temos um data set rico em preditora, mas limitado em validez quantitativa.

Isso torna a analise deste data set desafiadora, porém rica. As intepretassões dos resultados devem levar em consideração o seu tamanho, mas as respostas encontradas resultam em informações valiosas para o negócio.


Características do data Set:

- 55 variáveis:
* 5 numéricas
* 50 categóricas
- 350 respondentes
- Uma variável resposta: Output (que diz se o cliente irá realizar um novo pedido ou não)



```{r Data Raw, message=TRUE, warning=TRUE, paged.print=TRUE}
df_raw %>% glimpse()
```

## Analises Exploratórias


### Análise variáveis Categóricas:


```{r Analise exploratoria dados categoricos, echo=FALSE}
colunas_cat <- df %>%
  select(-latitude, -longitude, - Reviews, -Pin.code) %>%
  select(!starts_with("Output")) %>% 
  select(where(is.character)) %>% 
  colnames()

df %>%
    ggplot(aes(Gender, fill = Output_grafico)) +
    geom_bar(position = 'dodge')+
    labs(y = "Contagem",
       title = "Analise explorataria das categoricas",
       subtitle = "Objetivo é avaliar correlação com variável resposta ou inconsistência na variável",
       fill = "churn")+
    theme_classic()


faz_grafico <- function(x) {
  df %>%
    ggplot(aes(.data[[x]], fill = Output_grafico)) +
    geom_bar(position = 'dodge')+
    labs(y = "Contagem",
       title = "Analise explorataria das categoricas",
       subtitle = "Objetivo é avaliar correlação com variável resposta ou inconsistência na variável",
       caption = "Em dias",
       fill = "churn")+
    theme_classic()
}
(graficos_cat <- map(colunas_cat, faz_grafico))



```
### Análise variáveis numéricas

```{r Analise exploratoria dados continuos}
colunas_cont <- df %>%
  select(-latitude, -longitude, - Reviews, -Pin.code) %>%
  select(!starts_with("Output")) %>% 
  select(where(is.numeric)) %>% 
  colnames()

#variavel escala -3 a 3: Health.Concern
# " escala 0 a 6:
# Sem escala: Good.Taste

df %>%
    ggplot(aes(Good.Taste, fill = factor(Output))) +
    geom_bar(position = 'dodge')+
    labs(y = "Contagem",
       title = "Analise explorataria das continuas",
       subtitle = "Objetivo é avaliar correlação com variável resposta ou inconsistência na variável",
       fill = "churn")+
  scale_x_continuous(n.breaks = NULL)+
    theme_classic()

fazer_grafico_cont <- function(x){
  df %>%
    ggplot(aes(.data[[x]], fill = factor(Output))) +
    geom_bar(position = 'dodge')+
    labs(y = "Contagem",
       title = "Analise explorataria das continuas",
       subtitle = "Objetivo é avaliar correlação com variável resposta ou inconsistência na variável",
       fill = "churn")+
  scale_x_continuous(breaks = (-3:100))+
    theme_classic()
  
  
}

(graficos_cont <- map(colunas_cont, fazer_grafico_cont))

```



### Análise correlação:

```{r correlacao, echo=FALSE}
df %>% select(where(is.numeric)) %>% colnames()

correlacao <- round(cor(df %>% select(where(is.numeric))), 1)

#As 10 maiores correlações com a variável resposta (pode dar ruim se aumentar o número de colunas)


top10var <- data.frame(n = c(correlacao)) %>%
  slice_tail(n = 39) %>%
  mutate(col = df %>%select(where(is.numeric)) %>%colnames()) %>% 
  filter(n >= .2 |n <= -.2) %>% 
  select(col) %>% 
  as.list()


typeof(c(correlacao))

ggcorrplot(correlacao, hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726")) 
```

## Modelo: Previsão se o consumidor irá comprar novamente:

```{r Recipe, include=FALSE}
Receita <- recipe(Output ~ ., data = df) %>%
  step_select(-Reviews,
              -Pin.code,
              -latitude,
              -longitude,
              -Medium..P1.,
              -Perference.P1.,
              -Perference.P2.) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal()) %>%
  step_bin2factor(Output) %>% 
  step_nzv(all_numeric_predictors())

preparado <- prep(Receita, df)
assado <- bake(preparado, new_data = NULL)

split <- initial_split(assado)
train <- training(split)
test  <- testing(split)

```


### Modelo Knn:

```{r Modelo KNN}
library(FNN)

set.seed(7)

k_otimo <- 7

df_knn <- assado %>% 
  mutate(Output = ifelse(Output == 1, "yes", "no"))

fit <- knn.reg(train = train, 
               test = test, 
               y = train$Output, 
               k = k_otimo)  


```

### Regressão Linear

```{r Modelo Regressao logistica}
set.seed(7)

fit_glm <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification") %>%
  fit(Output~ ., train)

fitted_glm <- fit_glm %>%
  predict(new_data = test, type = "prob") %>%
  mutate(observado = test$Output,
         modelo = "logística")%>%
  mutate(observado_teste = ifelse(observado == "yes",1,0)) %>% 
  mutate(observado_modelo = ifelse(.pred_yes >=.5, 1, 0))

roc_glm <- fitted_glm %>%
  roc_auc(observado, .pred_yes)

rmse_glm <- fitted_glm %>%
  rmse(observado_teste, .pred_yes)

```
### Performance dos modelos


```{r Performance dos modelos}



```
