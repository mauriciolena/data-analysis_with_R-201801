# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serão carregadas junto ao tidyverse
library(tidyverse)
library(lubridate)



# Crie um dataframe com o conteúdo do arquivo ted_main.csv.gz. 

dfArquivo <- read_csv("aula-05/data/ted_main.csv.gz")



# Visualize o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas.
# As variáveis duration, film_date e published_date estão no tipo de dados apropriado?

summary(dfArquivo)

# Converta as seguintes variáveis utilizando o pacote Lubridate:
#     * duration, para duração (em segundos). Experimente utilizar as funções as.duration e duration. Mantenha aquela que considerar mais apropriada.
#     * film_date, para data, com a função as_datetime.
#     * published_date, para data, com a função as_datetime..

dfArquivo %<>%
  mutate( 
    duration = as.duration(duration),
    film_date = as_datetime(film_date),
    published_date = as_datetime(published_date))


# Converta as seguintes variáveis character para variáveis categóricas com a função factor.
#     * event
#     * speaker_occupation

dfArquivo %>%
  mutate(event = factor(event),
         speaker_occupation = factor(speaker_occupation))


# Retire do dataframe a variável name
dfArquivo %>%
  dfArquivo$name <- NULL
  head(dfArquivo)


# Visualize novamente o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas. 
# Verifique as contagens das variáveis categóricas

summary(dfArquivo)

# Verifique quais registros possuem a menor quantidade de línguas. Corrija para que possuam no mínimo 1 idioma.

dfArquivo%>%
  mutate (languages = (replace(languages, languages == 0, 1))) -> dfArquivo
  summary(dfArquivo)



# Verifique os 15 registros com menor data de filmagem. 

  head(dfArquivo%>%
    group_by(description) %>%
    summarise(min(film_date), sort = TRUE),15)
  

# Crie um dataframe com a contagem de apresentações por ano de filmagem e visualize todo o seu conteúdo

  dfArquivo%>%
    mutate(ano_filmagem = year(film_date)) %>%
    group_by(ano_filmagem) %>%
    summarise(qtd = n()) %>%
    ungroup() -> cont_apresentacoes 
  cont_apresentacoes %>% View()

  
  
  # Analise os 10 quantis da quantidade de apresentações por ano.
  # Descarte, do data frame de apresentações do TED Talks, aqueles cujo ano de filmagem tiver quantidade de apresentações menor ou igual à quantidade do quarto quantil.
  
  quarto_quantile <- quantile(cont_apresentacoes$ano_filmagem, probs = seq(0, 1, 0.1))[4]
  
  DataFrameTed %>%
    filter(year(film_date) > quarto_quantile) -> DataFrameTed_final
  
  
  
  # Verifique novamente o resumo dos dados do dataframe
  summary(DataFrameTed_final)
  
  
  
  # Verifique os 10 registros com maior duração.
  DataFrameTed_final %>%
    arrange(desc(duration)) %>%
    head(10) %>% 
    View()
  
  
  
  # Existem apresentações com duração maior que 3 desvios padrão acima da média? Liste elas
  DataFrameTed_final %>% 
    mutate(corte = mean(duration) + (3 * sd(duration))) %>%
    filter(as.numeric(duration) > corte) %>%
    View()
  
  
  
  # Calcule os 4 quartis e o IQR da duração das apresentações. 
  # Liste as apresentações cuja duração supera 1.5 * o IQR + o terceiro quartil
  
  terceiroQ <- quantile(as.numeric(DataFrameTed_final$duration, "seconds"))[3]
  iqrDuracao <- IQR(as.numeric(DataFrameTed_final$duration, "seconds"))
  
  DataFrameTed_final %>%
    filter(as.numeric(duration, "seconds") > (1.5 * iqrDuracao + terceiroQ)) %>%
    View()
  
  # Visualize os 10 quantis da quantidade de visualizações
  
  quantile(DataFrameTed_final$views, probs = seq(0, 1, 0.1))
  
  
  # Compare as seguintes estatísticas descritivas da quantidade de visualizações:
  #   * Média e Mediana. Qual é maior?
  #   * Desvio Absoluto da Mediana e Desvio Padrão. Qual é maior?
  #   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR é maior que o Desvio Absoluto da Mediana?
  #   * Com base na média e na mediana, e na razão entre o IQR e o Desvio Absoluto da Mediana, 
  #     você conclui que as quantidades de visualização estão distribuidas de forma simétrica em torno da média?
  
  media_views <- mean(DataFrameTed_final$views)
  mediana_views <- median(DataFrameTed_final$views)
  desv_padrao_views <- sd(DataFrameTed_final$views)
  desv_abs_med_views <- median(abs(DataFrameTed_final$views - median(DataFrameTed_final$views)))
  iqr_views <- IQR(DataFrameTed_final$views)
  
  cat("A média (", media_views, ") é maior que a mediana (", mediana_views, ")")
  cat("O desvio padrão (", desv_padrao_views, ") é maior que o desvio absoluto da mediana (", desv_abs_med_views, ")")
  cat("O IQR é ", iqr_views/desv_abs_med_views, " vezes maior que o desvio absolto da mediana")
  print("Há mais quantidades de visualizações que estão abaixo da média.")
  
  
  # Calcule a média, o desvio padrão, a mediana e o IQR da quantidade de línguas dos seguintes grupos:
  #     * 10% de vídeos com maior número de visualizações
  #     * 10% de vídeos com menor número de visualizações
  
  dez_por_cento <- DataFrameTed_final %>% count() * 0.1
  
  ted_main_final %>% 
    arrange(views) %>%
    head(dez_por_cento) -> DataFrameTed_visualizacoes
  
  cat("Média de línguas dos mais visualizados: ", mean(DataFrameTed_visualizacoes$languages)) 
  cat("Desvio padrão de línguas dos mais visualizados: ", sd(DataFrameTed_visualizacoes$languages))
  cat("Mediana de línguas dos mais visualizados: ", median(DataFrameTed_visualizacoes$languages))
  cat("IQR de línguas dos mais visualizados: ", IQR(DataFrameTed_visualizacoes$languages)) 
  
  
  DataFrameTed_final %>% 
    arrange(desc(views)) %>%
    head(dez_por_cento) -> menos_visualizacoes
  
  cat("Média de línguas dos menos visualizados: ", mean(menos_visualizacoes$languages)) 
  cat("Desvio padrão de línguas dos menos visualizados: ", sd(menos_visualizacoes$languages))
  cat("Mediana de línguas dos menos visualizados: ", median(menos_visualizacoes$languages))
  cat("IQR de línguas dos menos visualizados: ", IQR(menos_visualizacoes$languages)) 
  
  # Determine a quantidade de apresentações por evento cujo nome inicie com TED. 
  # Utilize a função str_detect para este filtro
  
  DataFrameTed_final %>%
    group_by(event) %>%
    filter(str_detect(event, "TED.*")) %>%
    summarise(qtd = n()) %>%
    ungroup() %>%
    View()
  
  
  # Determine, por evento cujo nome inicie com TED e que a quantidade de visualizações dos vídeos foi maior 
  # que a mediana calculada anteriormente.
  #   * a quantidade de apresentações resultante do filtro, por evento
  #   * o ano do evento (utilizar o menor ano da data de publicação)
  #   * a quantidade média de línguas das apresentações
  #   * o desvio padrão da quantidade de línguas
  #   * o coeficiente de variação da quantidade de línguas
  ### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÇÕES
  
  DataFrameTed_final %>%
    group_by(event) %>%
    filter(str_detect(event, "TED.*")) %>%
    filter(views > media_views) %>%
    summarise(qtd = n(), 
              ano = min(published_date), 
              media_linguas = mean(languages),
              desv_padrao_linguas = sd(languages),
              coef_var_linguas = sd(languages)/mean(languages)) %>%
    ungroup() %>%
    View()
  
  
  # Calcule e classifique as seguintes correlações
  #     * Quantidade de visualizações e Quantidade de línguas
  #     * Quantidade de visualizações e Duração
  #     * Quantidade de visualizações e Quantidade de Comentários
  #     * Quantidade de Comentários e Quantidade de línguas
  
  get_grau_correlacao <- function(x) {
    abs_x = abs(x)
    if (abs_x <= 0.3) {
      "Desprezível"
    } else if (abs_x <= 0.5) {
      "Fraca"
    } else if (abs_x <= 0.7) {
      "Moderada"
    } else if (abs_x <= 0.9) {
      "Forte"
    } else {
      "Muito forte"
    }
  }
  
  DataFrameTed_final %>%
    summarise(corr_views_languages = cor(views, languages),
              class_views_languages = get_grau_correlacao(corr_views_languages),
              corr_view_duration = cor(views, as.numeric(duration, "seconds")),
              class_views_duration = get_grau_correlacao(corr_view_duration),
              corr_views_comments = cor(views, comments),
              class_views_comments = get_grau_correlacao(corr_views_comments),
              corr_comments_languages = cor(comments, languages),
              class_comments_languages = get_grau_correlacao(corr_comments_languages))
  
  
  # Descarte os vídeos cuja duração seja maior que 3 desvios padrões da média. Calcule novamente as 5 correlações solicitadas
  DataFrameTed_final %>%
    mutate(corte = 3 * sd(as.numeric(duration, "seconds")) + mean(as.numeric(duration, "seconds"))) %>%
    filter(as.numeric(duration, "seconds") <= corte) %>%
    summarise(corr_views_languages = cor(views, languages),
              class_views_languages = get_grau_correlacao(corr_views_languages),
              corr_view_duration = cor(views, as.numeric(duration, "seconds")),
              class_views_duration = get_grau_correlacao(corr_view_duration),
              corr_views_comments = cor(views, comments),
              class_views_comments = get_grau_correlacao(corr_views_comments),
              corr_comments_languages = cor(comments, languages),
              class_comments_languages = get_grau_correlacao(corr_comments_languages))
  
  
  
  # Utilizando o data frame original, crie um dataframe com a mediana da duração dos vídeos por ano de filmagem. 
  # Calcule a correlação entre o ano e a mediana da duração e interprete o resultado
  
  DataFrameTed_final %>%
    mutate(duration = duration(duration, units = "seconds"),
           film_date = as_datetime(film_date)) %>%
    group_by(yearFilm = year(film_date)) %>%
    summarise(mediana_duracao = median(duration))  %>%
    ungroup() %>%
    summarise(cor(yearFilm, mediana_duracao))