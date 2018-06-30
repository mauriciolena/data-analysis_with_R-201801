# Descrição dos dados: https://tech.instacart.com/3-million-instacart-orders-open-sourced-d40d29ead6f2
# Estamos trabalhando com somente uma amostra do total de pedidos. O dataset abaixo não possui 3 milhões de pedidos ;)


library(tidyverse)
library(dplyr)


departments <- read_csv("project/departments.csv")                   # Cadastro de Departamentos
aisles <- read_csv("project/aisles.csv")                             # Cadastro de "Corredores"
products <- read_csv("project/products.csv")                         # Cadastro de Produtos

insta_orders <- read_csv( "project/orders_instacart.csv" )           # Amostra de pedidos de usuários
insta_products <- read_csv( "project/order_products_instacart.csv" ) # Produtos que compõe os pedidos

#1 # Quantos dos produtos do cadastro nunca foram comprados?

inner_join(insta_products,products, by = "product_id", type = "inner") -> Produtos_Comprados 
products %>% filter(!product_id %in% as.vector(Produtos_Comprados$product_id)) -> Produtos_NaoComprados
Produtos_NaoComprados%>% View()
Produtos_NaoComprados%>%count() %>% View()


#2 # Crie um dataframe com os dados combinados de produtos, corredores e departamentos. 

products %>%
  inner_join(aisles, by="aisle_id",type = "inner") %>%
  inner_join(departments, by="department_id",type = "inner")%>%
  arrange(aisle_id) -> Combinados_Produtos
Combinados_Produtos %>% View()


#3 # Quais as 10 combinações corredor + departamento que possuem mais produtos cadastrados? Use o dataframe da atividade #2.

Combinados_Produtos %>%
  select(aisle_id,department_id) %>%
  group_by(aisle_id,department_id) %>%
  count_() %>%
  arrange(desc(n)) %>%
  head(10) -> top_dez_comprados
top_dez_comprados%>%View()


#4 # Qual o percentual de pedidos que possuem algum produto dos pares 'corredor + departamento' da atividade anterior?

insta_products %>%
  inner_join(products, by = 'product_id', type = "inner") %>%
  inner_join(top_dez_comprados, by = c('aisle_id', 'department_id'), type = "inner") -> top_dez_ordenado 
# total de pedidos com top 10 categorias 210,134

insta_products %>% # Quantidade d pedidos total 1,384,617
  count_() -> total_insta_products

top_dez_comprados %>%  # quantidade de produtos top 10 categorias 10187
  summarise(sum(n)) -> soma_produtos_top10

top_dez_ordenado %>% # total 210,134
  # mutate( colunaAux = 1) %>%
  count_() -> Total_pedidos_top10 

perc_orders <- ((Total_pedidos_top10 * 100) / total_insta_products ) 
perc_orders %>% View()

#5 # Crie um novo dataframe de produtos em pedidos retirando aqueles produtos que não estão categorizados 
# (usar resultado das atividades 3 e 4)


top_dez_ordenado %>%
  left_join(aisles, by="aisle_id") %>%
  left_join(departments, by="department_id") %>%
  filter(department != "missing" | aisle != "missing") -> Filter_ordends
Filter_ordends %>% View()

#6 # Crie um dataframe que combine todos os dataframes através das suas chaves de ligação. Para produtos de pedidos,
# use o dataframe top_dez_ordenado
# Transforme as variáveis user_id, department e aisle em factor
# Transforme a variável order_hour_of_day em um factor ordenado (ordered)
# Este dataframe deverá ser utilizado em todas as atividades seguintes


# departments     
# aisles             
# products         
# insta_orders    
# insta_products  

top_dez_ordenado %>%
  inner_join(products, by = "product_id") %>%
  inner_join(aisles, by = c("aisle_id.x" = "aisle_id")) %>%
  inner_join(departments, by = c("department_id.x" = "department_id")) %>%
  inner_join(insta_orders, by = "order_id") %>%
  select(order_id,
         user_id,
         order_number,
         order_dow,
         order_hour_of_day,
         days_since_prior_order,
         product_id,
         product_name.x,
         aisle_id.x,
         aisle,
         department_id.x,
         department) %>%
  mutate(user_id = factor(user_id),
         department = factor(department),
         aisle = factor(aisle),
         order_hour_of_day = ordered(order_hour_of_day)) -> dfGeral

summary(dfGeral)


#7 # Identifique os 5 horários com maior quantidade de usuários que fizeram pedidos

dfGeral %>%
  distinct(order_hour_of_day, user_id) %>%
  group_by(order_hour_of_day) %>%
  summarise(qty = n()) %>%
  ungroup() %>%
  arrange(desc(qty)) %>%
  head(5) %>%
  pull(order_hour_of_day) -> Top_5_horarios



#8 # Quais os 15 produtos mais vendidos nestes 5 horários? Identifique os produtos e a quantidade total nestes horários (total geral, não por hora)


#9 # Calcule a média de vendas por hora destes 15 produtos ao longo do dia,
# e faça um gráfico de linhas mostrando a venda média por hora destes produtos. 
# Utilize o nome do produto para legenda da cor da linha.
# Você consegue identificar algum produto com padrão de venda diferente dos demais? 


#10 # Calcule as seguintes estatísticas descritivas sobre a quantidade de pedidos por dia, para cada hora do dia. O resultado final deve ser exibido para cada hora do dia:
# Média, Desvio Padrão, Mediana, Mínimo e Máximo
# Considerando os valores calculados, você acredita que a distribuição por hora é gaussiana? 


#11 # Faça um gráfico da média de quantidade de produtos por hora, com 1 desvio padrão para cima e para baixo em forma de gráfico de banda


#12 # Visualize um boxplot da quantidade de pedidos por hora nos 7 dias da semana. O resultado deve ter order_dow como eixo x.


#13 # Identifique, por usuário, o tempo médio entre pedidos


#14 # Faça um gráfico de barras com a quantidade de usuários em cada tempo médio calculado


#15 # Faça um gráfico de barras com a quantidade de usuários em cada número de dias desde o pedido anterior. Há alguma similaridade entre os gráficos das atividades 14 e 15? 


#16 # Repita o gráfico da atividade 14 mantendo somente os usuários com no mínimo 5 pedidos. O padrão se mantém?


#17 # O vetor abaixo lista todos os IDs de bananas maduras em seu estado natural.
# Utilizando este vetor, identifique se existem pedidos com mais de um tipo de banana no mesmo pedido.


#18 # Se existirem, pedidos resultantes da atividade 17, conte quantas vezes cada tipo de banana aparece nestes pedidos com mais de um tipo de banana.
# Após exibir os tipos de banana, crie um novo vetor de id de bananas contendo somente os 3 produtos de maior contagem de ocorrências


#19 # Com base no vetor criado na atividade 18, conte quantos pedidos de, em média, são feitos por hora em cada dia da semana. 


#20 # Faça um gráfico dos pedidos de banana da atividade 19. O gráfico deve ter o dia da semana no eixo X, a hora do dia no eixo Y, 
# e pontos na intersecção dos eixos, onde o tamanho do ponto é determinado pela quantidade média de pedidos de banana 
# nesta combinação de dia da semana com hora


#21 # Faça um histograma da quantidade média calculada na atividade 19, facetado por dia da semana


#22 # Teste se há diferença nas vendas por hora entre os dias 3 e 4 usando o teste de wilcoxon e utilizando a simulação da aula de testes

