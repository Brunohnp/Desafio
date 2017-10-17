# Desafio LuizaLabs
# Bruno Henrique Nunes Pereira

library(readr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(caret)
library(graphics)
library(TTR)
library(tseries)
library(forecast)
library(stringr)
library(rebus)
library(cluster)
library(clValid)

# Carregando os dados -----------------------------------------------------
df <- read_csv("~/R/desafio.csv",
               col_types = cols(capture_date = col_date(format = "%Y-%m-%d"),
                                process_date = col_date(format = "%Y-%m-%d"),
                                source_channel = col_number()))

# Verificando os dados #1 ----------------------------------------------------
summary(df)
str(df)

# Tratamento inicial dos dados --------------------------------------------
df$code <- as.factor(df$code)
df$category <- as.factor(df$category)
df$order_status <- as.factor(df$order_status)
df$process_status <- as.factor(df$process_status)
df$source_channel <- as.factor(df$source_channel)

# Verificando os dados #2 -------------------------------------------------
summary(df)

#Quantidade vendida por produto
ggplot(df, aes(x = code)) +
  geom_bar(fill = "dark blue", alpha = 0.5)

#Quantidade vendida por produto e category
ggplot(df, aes(x = code, fill = category)) +
  geom_bar(alpha = 0.5)
#nao utilizar

ggplot(df, aes(x = code, fill = source_channel)) +
  geom_bar(alpha = 0.5)
#nao utilizar

#Verificando price por produto
ggplot(df, aes(x = code, y= price)) +
  geom_boxplot()
#Price nao e unitario - Criar nova variavel

#Verificando liquid_cost por produto
ggplot(df, aes(x = code, y= liquid_cost)) +
  geom_boxplot()
#Price nao e unitario - Criar nova variavel

#verificando order_status
ggplot(df, aes(x = code, fill = order_status)) +
  geom_bar(position = "stack")

#Verificando process_status x order_status
ggplot(df, aes(x = order_status, fill = process_status)) +
  geom_bar() +
  coord_flip()

#Manter process_status: processado
#manter order_status: "entrega total", "entrega parcial", "em rota de entrega", "disponível para retirada"

# Criando novas variaveis -------------------------------------------------
#Preco unitario
df$priceUn <- df$price/df$quantity

#Lucro por unidade
df$profit <- df$priceUn - df$liquid_cost

#Percentual de Lucro
df$profitPercent <- df$profit/df$priceUn

#Percentual de Pis/Cofins
df$pis_cofinsPercent <- df$pis_cofins/df$price

#Percentual de icms
df$icmsPercent <- df$icms/df$price

#Percentual de tax_substitution
df$tax_substPercent <- df$tax_substitution/df$price

#Mes de captura
df$capture_month <- month(df$capture_date)

#Ano-Mes de captura
df$capture_ym <- format.Date(df$capture_date, format = "%Y-%m")

# Verificando os dados #3 ---------------------------------------------------

#Verificando quantidade por mes
ggplot(df, aes(x = code)) +
  geom_bar() +
  facet_grid(~capture_ym)
#Retirar junho - dados parciais

# Retirando os pedidos com problemas ou que nao devem ser considerados  -------------------------------------

df2 <- df %>%
  filter(capture_date <= "2017-05-31") %>%
  filter(!is.na(process_date)) %>%
  filter(order_status %in% c("entrega total", "entrega parcial", "em rota de entrega", "disponível para retirada"))

ggplot(df2, aes(x = order_status, fill = process_status)) +
  geom_bar() +
  coord_flip()

# Verificando os dados #4 -------------------------------------------------
summary(df2)

#Quantidade vendida por produto
ggplot(df2, aes(x = code)) +
  geom_bar(fill = "dark blue", alpha = 0.5)

#Quantidade vendida por produto e category
ggplot(df2, aes(x = code, fill = category)) +
  geom_bar(alpha = 0.5)
#nao utilizar

#Quantidade vendida por produto e source_channel
ggplot(df2, aes(x = code, fill = source_channel)) +
  geom_bar(alpha = 0.5)
#nao utilizar

#Verificando priceUn por produto
ggplot(df2, aes(x = code, y= priceUn)) +
  geom_boxplot()
#variacao do priceUn pode ser uma caracteristica a se considerar no produto

#Verificando liquid_cost por produto
ggplot(df2, aes(x = code, y= liquid_cost)) +
  geom_boxplot()
#liquid_cost nao tem variacao

# Visao orderId -----------------------------------------------------------
df.orderId <- df2 %>%
  group_by(order_id) %>%
  summarise(ProdutosOrder = n())

ggplot(df.orderId, aes(x = ProdutosOrder)) +
  geom_bar(fill = "dark blue", alpha = 0.5) +
  coord_flip()

#Market Basket Analysis  nao se aplica
#A maioria dos pedidos possuem apenas um produto

# Criando visao produto --------------------------------------------------
#As variaveis com # foram testadas mas não contribuiram para diferenciacao dos grupos
df.produtos <- df2 %>%
  group_by(code) %>%
  summarise(
    QtyTot = sum(quantity),
    #QtyMes = (sum(quantity)/12),
    PriceMean = mean(priceUn),
    #PriceMax = max(priceUn),
    #PriceMin = min(priceUn),
    #PriceVarPercent = (max(priceUn) - min(priceUn))/PriceMean,
    PriceSD = ifelse(is.nan(sd(priceUn)), 0, sd(priceUn)),
    #ProfitMean = mean(profit),
    ProfitPercent = mean(profitPercent),
    LiquidCost = mean(liquid_cost)
    #TaxTotalPercent = mean(pis_cofinsPercent + icmsPercent + tax_substPercent)
  )
#Visao mensal por produto
df.produtosMes <- df2 %>%
  group_by(code, capture_ym) %>%
  summarise(QtyMonth = sum(quantity)) %>%
  spread(key = capture_ym, value = QtyMonth)

df.produtosMes[is.na(df.produtosMes)] <- 0

#Visao trimestral dos produtos %
#df.produtosTri <- data.frame(code = df.produtosMes[,1],
#                             T3_16 = (df.produtosMes[,2] + df.produtosMes[,3] + df.produtosMes[,4])/df.produtos$QtyTot,
#                             T4_16 = (df.produtosMes[,5] + df.produtosMes[,6] + df.produtosMes[,7])/df.produtos$QtyTot,
#                             T1_17 = (df.produtosMes[,8] + df.produtosMes[,9] + df.produtosMes[,10])/df.produtos$QtyTot,
#                             T2_17 = (df.produtosMes[,11] + df.produtosMes[,12] + df.produtosMes[,13])/df.produtos$QtyTot
#                             )

#Visao trimestral dos produtos Quantidade
df.produtosTri <- data.frame(code = df.produtosMes[,1],
                             T3_16 = df.produtosMes[,2] + df.produtosMes[,3] + df.produtosMes[,4],
                             T4_16 = df.produtosMes[,5] + df.produtosMes[,6] + df.produtosMes[,7],
                             T1_17 = df.produtosMes[,8] + df.produtosMes[,9] + df.produtosMes[,10],
                             T2_17 = df.produtosMes[,11] + df.produtosMes[,12] + df.produtosMes[,13])

#df.produtos <- left_join(df.produtos, df.produtosMes, by = "code")
df.produtos <- left_join(df.produtos, df.produtosTri, by = "code")


# Criando os agrupamentos -------------------------------------------------
set.seed(1510)

#COM CENTER E SCALE
#Centralizando e colocando as variaveis na mesma escala
#df.produtosSC <- data.frame(scale(df.produtos[,2:10], center = TRUE, scale = TRUE))
#summary(df.produtosSC)
#Verificando as possiveis quantidades de grupos atraves de cluster hierarquico
#hcH <- hclust(dist(df.produtosSC))
#plot(hcH)
#Criando os agrupamentos com kmeans
#(testados k= 8:13, nstart= 15:25, center= T:F, scale= T:F) 
#hc <- kmeans(df.produtosSC, 12, nstart = 20)

#SEM CENTER E SCALE
#Verificando as possiveis quantidades de grupos atraves de cluster hierarquico
hcH <- hclust(dist(df.produtos[,2:10]))
plot(hcH)

#Criando os agrupamentos com kmeans
#(testados k= 8:13, nstart= 15:25, center= T:F, scale= T:F) 
hc <- kmeans(df.produtos[,2:10], 15, nstart = 20)

#Verificando as estatisticas do agrupamento
hc
hc$centers
hc$totss
hc$withinss #Minimizar a diferenca no cluster
hc$betweenss #Maximizar a diferenca entre clusters
hc$tot.withinss
hc$size

#Dunn's Index - Higher Dunn Be!er separated / more compact
dunn(clusters = hc$cluster, Data = df.produtos[,2:10])

#Verificando a diferenciacao dos clusters para cada variavel
plot(hc$centers)
df.centers <- data.frame(hc$centers)
plot(df.centers)
#Clusters mais proximos com relacao ao preco, variacao de preco e custo
#Clusters mais separados com relacao a quantidade total e vendas por trimestre

df.produtos$cluster <- hc$cluster
df3 <- left_join(df2, df.produtos[,c(1, 11)], by = "code")

# Verificando os dados #5 - Caracteristicas dos Clusters -------------------------------------------------
ggplot(df.produtos, aes(y = QtyTot, x = factor(cluster))) +
  geom_boxplot()

ggplot(df.produtos, aes(y = PriceMean, x = factor(cluster))) +
  geom_boxplot()

ggplot(df.produtos, aes(y = PriceSD, x = factor(cluster))) +
  geom_boxplot()

ggplot(df.produtos, aes(y = ProfitPercent, x = factor(cluster))) +
  geom_boxplot()

ggplot(df.produtos, aes(y = LiquidCost, x = factor(cluster))) +
  geom_boxplot()

ggplot(df.produtos, aes(y = X2016.06, x = factor(cluster))) +
  geom_boxplot()

ggplot(df.produtos, aes(y = X2016.09, x = factor(cluster))) +
  geom_boxplot()

ggplot(df.produtos, aes(y = X2016.12, x = factor(cluster))) +
  geom_boxplot()

ggplot(df.produtos, aes(y = X2017.03, x = factor(cluster))) +
  geom_boxplot()

hc$centers
  

# Criando a visao para Time Series ----------------------------------------
#Time series por cluster
df.TS <- df3 %>%
  select(capture_ym, cluster, quantity) %>%
  group_by(capture_ym, cluster) %>%
  summarise(QtyTot = sum(quantity)) %>%
  spread(key = cluster, value = QtyTot)

#Teste por produto
#df.TS <- df2 %>%
#  select(capture_ym, code, quantity) %>%
#  group_by(capture_ym, code) %>%
#  summarise(QtyTot = sum(quantity)) %>%
#  spread(key = code, value = QtyTot)

#Substituindo NAs por 0
df.TS[is.na(df.TS)] <- 0

#Criando time series 
df.ts <- ts(df.TS[,2:16], frequency = 12, start = c(2016,06))
plot.ts(df.ts[,1:8], lwd = 2)
plot.ts(df.ts[,9:15], lwd = 2)
#Nota-se o comportamento diferente para cada um dos clusters

#Verificando tendencia, sazonalidade e componente aleatorio 
decompose(df.ts[1:15])
#Periodo muito curto inviabiliza a analise

# Modelagem e previsao por Cluster ----------------------------------------
#Cluster 1
cluster1 <- auto.arima(df.ts[,1])
plot(forecast(cluster1))
cluster1

predClust1 <- predict(cluster1, n.ahead = 3)
predClust1
pred1 <- predClust1$pred / hc$size[1]
pred1

#Cluster 2
cluster2 <- auto.arima(df.ts[,2])
plot(forecast(cluster2))
cluster2

predClust2 <- predict(cluster2, n.ahead = 3)
predClust2
pred2 <- predClust2$pred / hc$size[2]
pred2

#Cluster 3
cluster3 <- auto.arima(df.ts[,3])
plot(forecast(cluster3))
cluster3

predClust3 <- predict(cluster3, n.ahead = 3)
predClust3
pred3 <- predClust3$pred / hc$size[3]
pred3

#Cluster 4
cluster4 <- auto.arima(df.ts[,4])
plot(forecast(cluster4))
cluster4

predClust4 <- predict(cluster4, n.ahead = 3)
predClust4
pred4 <- predClust4$pred / hc$size[4]
pred4

#Cluster 5
cluster5 <- auto.arima(df.ts[,5])
plot(forecast(cluster5))
cluster5

predClust5 <- predict(cluster5, n.ahead = 3)
predClust5
pred5 <- predClust5$pred / hc$size[5]
pred5

#Cluster 6
cluster6 <- auto.arima(df.ts[,6])
plot(forecast(cluster6))
cluster6

predClust6 <- predict(cluster6, n.ahead = 3)
predClust6
pred6 <- predClust6$pred / hc$size[6]
pred6

#Cluster 7
cluster7 <- auto.arima(df.ts[,7])
plot(forecast(cluster7))
cluster7

predClust7 <- predict(cluster7, n.ahead = 3)
predClust7
pred7 <- predClust7$pred / hc$size[7]
pred7

#Cluster 8
cluster8 <- auto.arima(df.ts[,8])
plot(forecast(cluster8))
cluster8

predClust8 <- predict(cluster8, n.ahead = 3)
predClust8
pred8 <- predClust8$pred / hc$size[8]
pred8

#Cluster 9
cluster9 <- auto.arima(df.ts[,9])
plot(forecast(cluster9))
cluster9

predClust9 <- predict(cluster9, n.ahead = 3)
predClust9
pred9 <- predClust9$pred / hc$size[9]
pred9

#Cluster 10
cluster10 <- auto.arima(df.ts[,10])
plot(forecast(cluster10))
cluster10

predClust10 <- predict(cluster10, n.ahead = 3)
predClust10
pred10 <- predClust10$pred / hc$size[10]
pred10

#Cluster 11
cluster11 <- auto.arima(df.ts[,11])
plot(forecast(cluster11))
cluster11

predClust11 <- predict(cluster11, n.ahead = 3)
predClust11
pred11 <- predClust11$pred / hc$size[11]
pred11

#Cluster 12
cluster12 <- auto.arima(df.ts[,12])
plot(forecast(cluster12))
cluster12

predClust12 <- predict(cluster12, n.ahead = 3)
predClust12
pred12 <- predClust12$pred / hc$size[12]
pred12

#Cluster 13
cluster13 <- auto.arima(df.ts[,13])
plot(forecast(cluster13))
cluster13

predClust13 <- predict(cluster13, n.ahead = 3)
predClust13
pred13 <- predClust13$pred / hc$size[13]
pred13

#Cluster 14
cluster14 <- auto.arima(df.ts[,14])
plot(forecast(cluster14))
cluster14

predClust14 <- predict(cluster14, n.ahead = 3)
predClust14
pred14 <- predClust14$pred / hc$size[14]
pred14

#Cluster 15
cluster15 <- auto.arima(df.ts[,15])
plot(forecast(cluster15))
cluster15

predClust15 <- predict(cluster15, n.ahead = 3)
predClust15
pred15 <- predClust15$pred / hc$size[15]
pred15

# Consolidando os resultados ----------------------------------------------
Previsoes <- ts.union(pred1,
                      pred2,
                      pred3,
                      pred4,
                      pred5,
                      pred6,
                      pred7,
                      pred8,
                      pred9,
                      pred10,
                      pred11,
                      pred12,
                      pred13,
                      pred14,
                      pred15,
                      dframe = TRUE)

Previsoes <- Previsoes %>% gather(cluster)
Previsoes$Mes <- c(201706, 201707, 201708)
Previsoes <- Previsoes %>% spread(key = Mes, value = value)
Previsoes$cluster <- as.integer(str_extract(Previsoes$cluster, pattern = one_or_more(DIGIT)))

df.ProdPrev <- left_join(df.produtos, Previsoes, by = "cluster")
df.ProdPrev <- df.ProdPrev %>% select(code, `201706`, `201707`, `201708`)

df.ProdPrevVerificacao <- left_join(df.produtosMes, df.ProdPrev, by = "code")

# Gerando .csv ------------------------------------------------------------
write.csv(df.ProdPrev, file = "PrevisaoPorProduto.csv", row.names = FALSE)
