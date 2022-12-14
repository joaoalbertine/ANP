#Prova Prática#
#João Paulo França Albertine


#biblioteca utilizadas

install.packages("patchwork")
install.packages("dplyr")
install.packages("data.table")
install.packages("ggplot2")

#carregando as bibliotecas

library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)
library(patchwork)

#lendo o arquivo

anp_g <- as.data.table(read_excel("C:\\Users\\João Paulo\\Desktop\\Processo Seletivo\\dados_desafiodatascientistintern_vendas_distribuidoras_anp.xlsx",
                    sheet = "gasolina"))


anp_e <- as.data.table(read_excel("C:\\Users\\João Paulo\\Desktop\\Processo Seletivo\\dados_desafiodatascientistintern_vendas_distribuidoras_anp.xlsx",
                    sheet = "etanol"))


anp_d <- as.data.table(read_excel("C:\\Users\\João Paulo\\Desktop\\Processo Seletivo\\dados_desafiodatascientistintern_vendas_distribuidoras_anp.xlsx",
                    sheet = "diesel"))


#alterações para possibilitar as análises

anp_g_long <- copy(melt(anp_g, id.vars = c("regiao","meses"),variable.name = "ano",value.name = "quantidade")) 

anp_e_long <- copy(melt(anp_e, id.vars = c("regiao","meses"),variable.name = "ano",value.name = "quantidade")) 

anp_d_long <- copy(melt(anp_d, id.vars = c("regiao","meses"),variable.name = "ano",value.name = "quantidade")) 


# Dummy para os trimestres

anp_g_long <- anp_g_long %>%
  mutate(T1 = ifelse(meses %in% c(1, 2, 3), 1, 0),
         T2 = ifelse(meses %in% c(4, 5, 6), 1, 0),
         T3 = ifelse(meses %in% c(7, 8, 9), 1, 0),
         T4 = ifelse(meses %in% c(10, 11, 12), 1, 0))

anp_e_long <- anp_e_long %>%
  mutate(T1 = ifelse(meses %in% c(1, 2, 3), 1, 0),
         T2 = ifelse(meses %in% c(4, 5, 6), 1, 0),
         T3 = ifelse(meses %in% c(7, 8, 9), 1, 0),
         T4 = ifelse(meses %in% c(10, 11, 12), 1, 0))

anp_d_long <- anp_d_long %>%
  mutate(T1 = ifelse(meses %in% c(1, 2, 3), 1, 0),
         T2 = ifelse(meses %in% c(4, 5, 6), 1, 0),
         T3 = ifelse(meses %in% c(7, 8, 9), 1, 0),
         T4 = ifelse(meses %in% c(10, 11, 12), 1, 0))

#coluna meses as a factor

anp_g_long[, meses := factor(meses, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))]

anp_d_long[, meses := factor(meses, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))]

anp_e_long[, meses := factor(meses, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))]

#criando a coluna data

anp_g_long[, data := as.Date(paste0(ano,"-" ,meses,"-01"))]

anp_d_long[, data := as.Date(paste0(ano,"-" ,meses,"-01"))]

anp_e_long[, data := as.Date(paste0(ano,"-" ,meses,"-01"))]


#### Gasolina ####

#criando um modelo trimestral para analise de sazonalidade


modelo_tri_br <-lm(quantidade ~ data + T1 +T2 + T3, anp_g_long[regiao == "br"])

modelo_tri_df <-lm(quantidade ~ data + T1 +T2 + T3, anp_g_long[regiao == "df"])

modelo_tri_sp <-lm(quantidade ~ data + T1 +T2 + T3, anp_g_long[regiao == "sp"])

modelo_tri_to <-lm(quantidade ~ data + T1 +T2 + T3, anp_g_long[regiao == "to"])

modelo_tri_mg <-lm(quantidade ~ data + T1 +T2 + T3, anp_g_long[regiao == "mg"])

modelo_tri_pa <-lm(quantidade ~ data + T1 +T2 + T3, anp_g_long[regiao == "pa"])

modelo_tri_mt <-lm(quantidade ~ data + T1 +T2 + T3, anp_g_long[regiao == "mt"])

modelo_tri_ma <-lm(quantidade ~ data + T1 +T2 + T3, anp_g_long[regiao == "ma"])

modelo_tri_go <-lm(quantidade ~ data + T1 +T2 + T3, anp_g_long[regiao == "go"])


summary(modelo_tri_br)

summary(modelo_tri_sp)

summary(modelo_tri_ma)

#O estado de são paulo apresenta sazonalidade a 5% significância no terceiro trimestre 
#O estado do maranhão apresenta para o 1 e 2 trimestre

#modelo mensal

modelo_mes <-lm(quantidade ~ data + meses, anp_g_long[regiao =="br"])

summary(modelo_mes)


#plotando gráficos

#análise de todas as regiões
ggplot(data = anp_g_long, aes(x = data, y = quantidade, color = regiao)) +
  geom_line() +
  labs(x = "tempo",  y = "Quantidade") +
  theme_classic()+
  facet_wrap(~regiao)

ggplot(data = anp_g_long, aes(x = data, y = quantidade, color = regiao)) +
  geom_line() +
  labs(x = "tempo",  y = "Quantidade") +
  theme_classic()
  #facet_wrap(~regiao)

#linha de tendência para a gasolina Brasil
gasolina<-ggplot(data = anp_g_long[regiao == "br"], aes(x = data, y = quantidade)) +
  geom_line(color ="indianred1" ) +
  geom_smooth(se=FALSE, method = "loess")+
  labs(x = "tempo",  y = "Quantidade") 
  theme_classic()

#últimos 5 anos
gasolina5anos<-ggplot(data = anp_g_long[data >= as.Date("2015-01-01")][regiao == "br"], aes(x = data, y = quantidade)) +
    geom_line(color ="indianred1" ) +
    geom_smooth(method = "loess", se = FALSE)+  
    #geom_smooth(se=FALSE, method = "lm")#+
    labs(title = "Tendência Gasolina",x = "tempo",  y = "Quantidade") 
    theme_classic()  
  
  
#linha de tendência para a gasolina São Paulo
ggplot(data = anp_g_long[regiao == "sp"], aes(x = data, y = quantidade)) +
  geom_line(color ="indianred1" ) +
  geom_smooth(se=FALSE, method = "loess")+
  labs(x = "tempo",  y = "Quantidade") +
  theme_classic()

#### Diesel ####

#criando um modelo trimestral para analise de sazonalidade


modelo_tri_diesel_br <-lm(quantidade ~ data + T1 +T2 + T3, anp_d_long[regiao == "br"])

modelo_tri_diesel_df <-lm(quantidade ~ data + T1 +T2 + T3, anp_d_long[regiao == "df"])

modelo_tri_diesel_sp <-lm(quantidade ~ data + T1 +T2 + T3, anp_d_long[regiao == "sp"])

modelo_tri_diesel_to <-lm(quantidade ~ data + T1 +T2 + T3, anp_d_long[regiao == "to"])

modelo_tri_diesel_mg <-lm(quantidade ~ data + T1 +T2 + T3, anp_d_long[regiao == "mg"])

modelo_tri_diesel_pa <-lm(quantidade ~ data + T1 +T2 + T3, anp_d_long[regiao == "pa"])

modelo_tri_diesel_mt <-lm(quantidade ~ data + T1 +T2 + T3, anp_d_long[regiao == "mt"])

modelo_tri_diesel_ma <-lm(quantidade ~ data + T1 +T2 + T3, anp_d_long[regiao == "ma"])

modelo_tri_diesel_go <-lm(quantidade ~ data + T1 +T2 + T3, anp_d_long[regiao == "go"])


summary(modelo_tri_diesel_br)

summary(modelo_tri_diesel_sp)

summary(modelo_tri_diesel_ma)

#aboserva-se diferença na sazonalidade do estado do maranhão para o estado de são paulo#
#São Paulo apresenta maior tendência para o primeiro e terceiro trimestre e Maranhão para o segundo e terceiro.

#modelo mensal
modelo_mes_diesel <-lm(quantidade ~ data + meses, anp_d_long[regiao =="br"])

summary(modelo_mes_diesel)


#plotando gráficos

#análise de todas as regiões
ggplot(data = anp_d_long, aes(x = data, y = quantidade, color = regiao)) +
  geom_line() +
  labs(x = "tempo",  y = "Quantidade") +
  theme_classic()+
  facet_wrap(~regiao)

ggplot(data = anp_d_long, aes(x = data, y = quantidade, color = regiao)) +
  geom_line() +
  labs(x = "tempo",  y = "Quantidade") +
  theme_classic()
#facet_wrap(~regiao)

#linha de tendência para a diesel Brasil
diesel<-ggplot(data = anp_d_long[regiao == "br"], aes(x = data, y = quantidade)) +
  geom_line(color ="indianred1" ) +
  geom_smooth(se=FALSE, method = "loess")+
  labs(x = "tempo",  y = "Quantidade") 
theme_classic()

#últimos 5 anos
diesel5anos<-ggplot(data = anp_d_long[data >= as.Date("2015-01-01")][regiao == "br"], aes(x = data, y = quantidade)) +
  geom_line(color ="indianred1" ) +
  geom_smooth(method = "loess", se = FALSE)+  
  #geom_smooth(se=FALSE, method = "lm")#+
  labs(title = "Tendência Diesel",x = "tempo",  y = "Quantidade") 
theme_classic()  


#linha de tendência para a diesel São Paulo
ggplot(data = anp_d_long[regiao == "sp"], aes(x = data, y = quantidade)) +
  geom_line(color ="indianred1" ) +
  geom_smooth(se=FALSE, method = "loess")+
  labs(x = "tempo",  y = "Quantidade") +
  theme_classic()

#### Etanol ####

#criando um modelo trimestral para analise de sazonalidade

modelo_tri_etanol_br <-lm(quantidade ~ data + T1 +T2 + T3, anp_e_long[regiao == "br"])

modelo_tri_etanol_df <-lm(quantidade ~ data + T1 +T2 + T3, anp_e_long[regiao == "df"])

modelo_tri_etanol_sp <-lm(quantidade ~ data + T1 +T2 + T3, anp_e_long[regiao == "sp"])

modelo_tri_etanol_to <-lm(quantidade ~ data + T1 +T2 + T3, anp_e_long[regiao == "to"])

modelo_tri_etanol_mg <-lm(quantidade ~ data + T1 +T2 + T3, anp_e_long[regiao == "mg"])

modelo_tri_etanol_pa <-lm(quantidade ~ data + T1 +T2 + T3, anp_e_long[regiao == "pa"])

modelo_tri_etanol_mt <-lm(quantidade ~ data + T1 +T2 + T3, anp_e_long[regiao == "mt"])

modelo_tri_etanol_ma <-lm(quantidade ~ data + T1 +T2 + T3, anp_e_long[regiao == "ma"])

modelo_tri_etanol_go <-lm(quantidade ~ data + T1 +T2 + T3, anp_e_long[regiao == "go"])


summary(modelo_tri_etanol_br)

summary(modelo_tri_etanol_sp)

summary(modelo_tri_etanol_ma)

# São paulo possui maior presença de sazonalidade no primeiro trimestre e com menos força no segundo
# Não se observa sazonalidade para o maranhão
# a nível nacional a presença de sacionalidade fica no 1 e 2 trimestre

#modelo mensal

modelo_mes_etanol <-lm(quantidade ~ data + meses, anp_e_long[regiao =="br"])

summary(modelo_mes_etanol)


#plotando gráficos

#análise de todas as regiões
ggplot(data = anp_e_long, aes(x = data, y = quantidade, color = regiao)) +
  geom_line() +
  labs(x = "tempo",  y = "Quantidade") +
  theme_classic()+
  facet_wrap(~regiao)

ggplot(data = anp_e_long, aes(x = data, y = quantidade, color = regiao)) +
  geom_line() +
  labs(x = "tempo",  y = "Quantidade") +
  theme_classic()
#facet_wrap(~regiao)

#linha de tendência para a etanol Brasil
etanol<-ggplot(data = anp_e_long[regiao == "br"], aes(x = data, y = quantidade)) +
  geom_line(color ="indianred1" ) +
  geom_smooth(se=FALSE, method = "loess")+
  labs(x = "tempo",  y = "Quantidade") 
theme_classic()

#últimos 5 anos
etanol5anos<-ggplot(data = anp_e_long[data >= as.Date("2015-01-01")][regiao == "br"], aes(x = data, y = quantidade)) +
  geom_line(color ="indianred1" ) +
  geom_smooth(method = "loess", se = FALSE)+  
  #geom_smooth(se=FALSE, method = "lm")#+
  labs(title ="Tendência Etanol",x = "tempo",  y = "Quantidade") 
theme_classic()  


#linha de tendência para a etanol São Paulo
ggplot(data = anp_e_long[regiao == "sp"], aes(x = data, y = quantidade)) +
  geom_line(color ="indianred1" ) +
  geom_smooth(se=FALSE, method = "loess")+
  labs(title =" Tendência Etanol SP",x = "tempo",  y = "Quantidade") +
  theme_classic()


###Tendência Brasil dos últimos 5 anos para todos os combustíveis

gasolina5anos / diesel5anos / etanol5anos
  plot_annotation(
    title = "Tendência Brasil 2015-2020")

###Tendência Brasil 2000-2020 para todos os combustíveis
  
gasolina / diesel / etanol
  plot_annotation(
  title = "Tendência Brasil 2000-2020")
  
#### Soma dos mercados de gasolina,diesel e etanol ####

#criando a data base anp_total para abrigar os valores necessários
  
anp_total <- anp_d_long$quantidade + anp_e_long$quantidade + anp_g_long$quantidade

anp_total <- as.data.table(anp_total)

anp_total[, data := as.Date(anp_total,format="%m/%d/%Y")]

anp_total$data<- anp_d_long$data

anp_total[,"regiao"] <- anp_d_long$regiao

#Criando gráfico com dados da anp

anptotal<-ggplot(data = anp_total[regiao == "br"], aes(x = data, y = anp_total)) +
  geom_line(color ="indianred1" ) +
  geom_smooth(method = "loess", se = FALSE)+  
  #geom_smooth(se=FALSE, method = "lm")#+
  labs(title ="Tendência Mercado 2015-2022",x = "tempo",  y = "Quantidade") 
  theme_classic()  

#Gráfico Crescimento Mercado e seu tamanho
  
print(anptotal)
