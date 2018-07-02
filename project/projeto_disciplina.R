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
  head(5) -> Top_5_horarios 
Top_5_horarios %>% View()


#8 # Quais os 15 produtos mais vendidos nestes 5 horários? Identifique os produtos e a quantidade total nestes horários (total geral, não por hora)

Top_5_horarios %>%
  inner_join(insta_orders, by = "order_hour_of_day") %>%
  inner_join(insta_products, by = "order_id") %>%
  select(product_id, order_hour_of_day) %>%
  group_by(order_hour_of_day) %>%
  count(product_id) %>%
  arrange(desc(n)) %>%
  head(15) -> Top_15_produtos


#9 # Calcule a média de vendas por hora destes 15 produtos ao longo do dia,
# e faça um gráfico de linhas mostrando a venda média por hora destes produtos. 
# Utilize o nome do produto para legenda da cor da linha.
# Você consegue identificar algum produto com padrão de venda diferente dos demais? 

Top_15_horarios%>%
  inner_join(products, by = 'product_id') %>%
  inner_join(insta_products, by = 'product_id') %>%
  inner_join(insta_orders, by = 'order_id') %>%
  group_by(product_id,product_name,order_hour_of_day.x,order_dow) %>%
  summarise(qtd_hora = n()) %>%
  ungroup() %>%
  group_by(product_id, product_name, order_hour_of_day.x) %>%
  summarise(media_qtd_hora = mean(qtd_hora)) %>%
  ungroup() -> orders_product_x_hour

ggplot(data = orders_product_x_hour,
       aes(x = order_hour_of_day.x,
           y = media_qtd_hora,
           group = product_name)
) +
  geom_line(aes(color = product_name)) +
  geom_point(aes(color = product_name)) +
  scale_y_continuous(breaks = seq(from = 0, to = 30, by = 2)) +
  labs(x = 'Hora',
       y = 'Quantidade',
       title = 'Top 15 Produtos por Hora',
       colour = 'Produto')

#10 # Calcule as seguintes estatísticas descritivas sobre a quantidade de pedidos por dia, para cada hora do dia. O resultado final deve ser exibido para cada hora do dia:
# Média, Desvio Padrão, Mediana, Mínimo e Máximo
# Considerando os valores calculadoNs, você acredita que a distribuição por hora é gaussiana? 

dfGeral %>%
  distinct(order_id, order_dow, order_hour_of_day) %>%
  mutate(qty = 1) %>%
  group_by(order_dow, order_hour_of_day) %>%
  summarise(total = sum(qty)) %>%
  group_by(order_hour_of_day) %>%
  summarise(mean(total), sd(total), median(total), min(total), max(total)) %>%
  ungroup()
#Devido ao fato de crescer e posteriormente descer, tem o comportamento de uma distribuição gausssiana


#11 # Faça um gráfico da média de quantidade de produtos por hora, com 1 desvio padrão para cima e para baixo em forma de gráfico de banda

dfGeral %>%
  mutate(quantidade = 1, order_hour_of_day = as.numeric(order_hour_of_day)) %>%
  group_by(product_id, order_hour_of_day) %>%
  summarise(total = sum(quantidade)) %>%
  ungroup() %>%
  group_by(order_hour_of_day) %>%
  mutate(low = mean(total) - sd(total), 
        high = mean(total) + sd(total)) %>%
  ungroup() %>%
  ggplot(aes(x = order_hour_of_day, 
             y = total, 
             ymin = low, 
             ymax = high)) +
  geom_ribbon(fill = "lightgray", 
              alpha = 0.5) +
  geom_jitter(alpha = .2, 
              height = 0, 
              width = 0.3) +
  theme_bw()


#12 # Visualize um boxplot da quantidade de pedidos por hora nos 7 dias da semana. O resultado deve ter order_dow como eixo x.

dfGeral %>%
  mutate(quantidade = 1) %>%
  group_by(order_dow, 
          order_hour_of_day) %>%
  summarise(total = sum(quantidade)) %>%
  ggplot(
    aes(
      x = order_dow, 
      y = total, 
      group = order_dow)) +
  geom_boxplot() +
  scale_x_continuous(breaks = 0:6) +
  theme_bw()

#13 # Identifique, por usuário, o tempo médio entre pedidos


dfGeral %>%
  group_by(user_id) %>%
  summarise(mean_prior_order = mean(days_since_prior_order)) %>%
  ungroup() %>%
  arrange(user_id)

#14 # Faça um gráfico de barras com a quantidade de usuários em cada tempo médio calculado

dfGeral %>%
  group_by(user_id) %>%
  summarise(mean_prior_order = mean(days_since_prior_order)) %>%
  ggplot(aes(x = mean_prior_order)) +
  geom_bar(fill = "grey", color = "black", alpha = 0.6) +
  scale_x_continuous(breaks = 0:30)

#15 # Faça um gráfico de barras com a quantidade de usuários em cada número de dias desde o pedido anterior. Há alguma similaridade entre os gráficos das atividades 14 e 15? 


dfGeral %>%
  group_by(days_since_prior_order) %>%
  ggplot(aes(x = days_since_prior_order)) +
  geom_bar(fill = "grey", color = "grey", alpha = 0.6) +
  scale_x_continuous(breaks = 0:30)

#Existe uma similaridade com relação ao intervalo de tempo.

#16 # Repita o gráfico da atividade 14 mantendo somente os usuários com no mínimo 5 pedidos. O padrão se mantém?

dfGeral %>%
  group_by(user_id) %>%
  summarise(orders = n()) %>%
  filter(orders >= 5) %>%
  pull(user_id) -> users_orders_5
  users_orders_5 %>% View()
  

dfGeral %>%
  filter(user_id %in% users_orders_5) %>%
  group_by(user_id) %>%
  summarise(total = mean(days_since_prior_order)) %>%
  ggplot(aes(x = total)) +
  geom_bar(fill = "blue", color = "blue", alpha = 0.6) +
  scale_x_continuous(breaks = 0:30)
# o padrão visualmente se mantém o mesmo.


#17 # O vetor abaixo lista todos os IDs de bananas maduras em seu estado natural.
# Utilizando este vetor, identifique se existem pedidos com mais de um tipo de banana no mesmo pedido.
banana <- c(13176, 24852, 29259, 37067, 39276)

insta_products %>%
  filter(product_id %in% banana) %>%
  group_by(order_id) %>%
  summarise(qty = n()) %>%
  ungroup() %>%
  filter(qty > 1) %>%
  pull(order_id) -> vendas_banana
  vendas_banana %>% View()

#18 # Se existirem, pedidos resultantes da atividade 17, conte quantas vezes cada tipo de banana aparece nestes pedidos com mais de um tipo de banana.
# Após exibir os tipos de banana, crie um novo vetor de id de bananas contendo somente os 3 produtos de maior contagem de ocorrências

insta_products %>%
  filter(product_id %in%  banana & order_id %in% vendas_banana) %>%
  distinct(order_id, product_id) %>%
  group_by(product_id) %>%
  summarise(quantidade = n()) %>%
  arrange(desc(quantidade)) %>%
  head(3) %>%
  pull(product_id) -> top_banana

#19 # Com base no vetor criado na atividade 18, conte quantos pedidos de, em média, são feitos por hora em cada dia da semana. 

products %>%
  filter(product_id %in% top_banana) %>%
  inner_join(insta_products, by = "product_id") %>%
  inner_join(insta_orders, by = "order_id") %>%
  distinct(order_dow, order_hour_of_day, order_id) %>%
  mutate(quantidade = 1) %>%
  group_by(order_dow, order_hour_of_day) %>%
  summarise(total = mean(sum(quantidade))) %>%
  ungroup()

#20 # Faça um gráfico dos pedidos de banana da atividade 19. O gráfico deve ter o dia da semana no eixo X, a hora do dia no eixo Y, 
# e pontos na intersecção dos eixos, onde o tamanho do ponto é determinado pela quantidade média de pedidos de banana 
# nesta combinação de dia da semana com hora


#21 # Faça um histograma da quantidade média calculada na atividade 19, facetado por dia da semana


#22 # Teste se há diferença nas vendas por hora entre os dias 3 e 4 usando o teste de wilcoxon e utilizando a simulação da aula de testes

