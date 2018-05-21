### Atividade prática

## Vamos começar carregando o arquivo de dados preparado para esta aula
library(tidyverse)

salarios <- read_csv("aula-03/data/201802_dados_salarios_servidores.csv.gz")


### 1 ####
## 
## O arquivo possui 2 colunas de Remuneração, uma em Reais e outra em Dólares. 
## Crie uma nova coluna de Remuneração Final que terá a soma entre a remuneração em Reais e as remuneração em Dólares convertida para Reais.
## Atenção: Para conversão, utilize a Cotação Comercial do Dólar no último dia útil de Fevereiro.
## Após criar esta coluna, descarte todos os registros cuja Remuneração Final for menor que R$ 900,00
## 
### # ####

dolar <- 3.2443  # cotação dia 28/02/2018- compra
# head(salarios, 20)

salarios %>%
  mutate(remuneracao_final = REMUNERACAO_REAIS + (REMUNERACAO_DOLARES * dolar)) -> subset_remuneracao_final


subset_remuneracao_final %>%
  filter(remuneracao_final > 900) -> subset_remuneracao_filter 

### 2 ####
## 
## Neste dataset é possível identificar que alguns servidores estão lotados em órgãos diferentes do seu órgão de exercício.
## Identifique os 5 cargos com maior quantidade de servidores que estão lotados em um órgão diferente, listando a descrição do cargo e a quantidade de servidores por cargo.
## Além de listar os 5 cargos e as quantidades, crie um vetor com os nomes destes 5 cargos. Crie este vetor com o nome de cargos_diferente_lotacao.
## 
## Dica: a função pull() do dplyr extrai uma variável em formato de vetor.
salarios %>% count(UF_EXERCICIO) %>% pull(UF_EXERCICIO) -> ufs # EXEMPLO
## 
### # ####

head (salarios %>%
  filter(ORGSUP_EXERCICIO != ORGSUP_LOTACAO) %>%
  count(DESCRICAO_CARGO, sort = TRUE),5)  -> cargos_diferente_lotacao
cargos_diferente_lotacao



### 3 ####
## 
## Utilizando o vetor criado na atividade anterior, calcule a média e o desvio padrão de cada cargo, 
## separando entre aqueles que estão lotados em outro órgão e aqueles que estão lotados no mesmo órgão de 
# exercício.
## O resultado deve conter:
##    - a descrição do cargo, 
##    - uma variável indicando se o servidor está lotado no mesmo órgão de exercício ou em um órgão diferente, 
##    - a média salarial
##    - o desvio padrão
##    - a mediana
##    - o desvio absoluto da mediana
##    - o menor salário
##    - o maior salário
## Analise os valores por lotação dentro de um mesmo cargo e comente ao final do exercício se você considera
# alguma diferença significativa.
## 
## Dica 1: o operador %in% testa se valores de uma variável pertencem ao conjunto de valores de um vetor. 
# Lembre que deve ser utilizada a variável cargos_diferente_lotacao
salarios %>% filter(DESCRICAO_CARGO %in% c("MINISTRO DE PRIMEIRA CLASSE", "ANALISTA DE TEC DA INFORMACAO", "PESQUISADOR")) %>% count(DESCRICAO_CARGO) # EXEMPLO
## Dica 2: Será necessário agrupar (group_by) por mais de uma variável para calcular as estatísticas solicitadas. 
## A função group_by permite múltiplos nomes de variáveis na mesma chamada.
## 
### # ####

salarios %>%
  mutate(max = max(salarios$REMUNERACAO_REAIS)) %>%
  mutate(min =  min(salarios$REMUNERACAO_REAIS)) %>%
  mutate(orgao = if_else(ORGSUP_EXERCICIO != ORGSUP_LOTACAO,"Orgão diferente", "Orgão igual")) %>%
  group_by(DESCRICAO_CARGO, orgao) %>%
  filter(DESCRICAO_CARGO %in%  c(cargos_diferente_lotacao%>% pull(DESCRICAO_CARGO))) %>%
  summarise(desvio_padrao = sd(salarios$REMUNERACAO_REAIS), 
            media = mean(salarios$REMUNERACAO_REAIS), 
            cv = desvio_padrao / media, 
            qtd_servidores = n(),
            max(REMUNERACAO_REAIS),
            min(REMUNERACAO_REAIS))


###########################################################################################################################
  
#  mean(REMUNERACAO_REAIS)
#  sd( subset_salarios$REMUNERACAO_REAIS )
#  median( subset_salarios$REMUNERACAO_REAIS )
#  (dam_salario <- median( abs( subset_salarios$REMUNERACAO_REAIS - median( subset_salarios$REMUNERACAO_REAIS ))))
#  min(subset_salarios$REMUNERACAO_REAIS)
#  max(subset_salarios$REMUNERACAO_REAIS)
#  salarios %>%
#  mutate(media = mean(REMUNERACAO_REAIS)) -> media_salario
#  salarios %>%
#  filter(DESCRICAO_CARGO %in%  c(cargos_diferente_lotacao%>% pull(DESCRICAO_CARGO))) -> filtro_cargos_diferentes
#  filtro_cargos_diferentes %>%count(DESCRICAO_CARGO)


