#############################################################################################
# Rotina do algoritmo para construir o indicador de progressividade da estrutura tributária # 
#############################################################################################

# Autor: Nikolas Schiozer
# Contato p/ dúvidas ou buggs: n.schiozer@usp.br

# Instalando pacotes _______________________________________________________________________________________________________________________________


install.packages("xlsx", dependencies = TRUE)
install.packages("readxl", dependencies = TRUE)
install.packages('forecast', dependencies = TRUE)
install.packages('Rcpp')

library(Rcpp)
library(xlsx)
library(readxl)
library(ggplot2)
library(forecast)

# Definindo o diretório de trabalho --------------------------------------------------------------------------------------------------------------------

# Defina o como diretório de trabalho o local onde estão armazenados os scripts e a planilha "R"

setwd("C:/Users/Nikolas/OneDrive/Documentos/Documentos/Projeto_MADE")

# Load as planilhas em Excel _______________________________________________________________________________________________________________________________


renda  <- read_excel("R.xlsx", sheet = "renda",col_names = FALSE)
aliq <- read_excel("R.xlsx", sheet=3)
base <- read_excel("R.xlsx", sheet=4)
pop.total <- read_excel("R.xlsx", sheet=5)


names(renda) <- c("year", "value")

# Cálculo da POP total

# Necessário rodar script "Projeção POP economicamente ativa"


forecast <- forecast(auto.arima(percentage.pia),h=12) # Spline até 2010. Anos seguintes são estimados por modelo ARIMA
forecast$mean

forecast.pop <- forecast(auto.arima(pop.total$Pop),h=4) #  Dados populacionais disponíveis até 2018. Anos seguintes projetados por modelo ARIMA

pop.total.2021 <- c(pop.total$Pop,forecast.pop$mean) # Pop total 2021

PIA <- pop.total.2021 * c(percentage.pia,as.numeric(forecast$mean))/100 # Tenho a renda até 2020. 2021 

renda2022 <- renda$value[length(renda$value)]


pop.anos <- data.frame( anos ,  PIA ) # Data.frame para anos e PIA
names(pop.anos) <- c("year", "pop")

renda.1 <- c(renda$value, renda2022)

renda.anos <- data.frame(anos,renda.1)

anos <- c(renda$year , 2022)

##### DOwnload renda e ano se desejado #####

install.packages("writexl")
library("writexl")
write_xlsx(renda.anos,"C:/Users/Nikolas/OneDrive/Documentos/Documentos/Projeto_MADE/renda.anos.xlsx")
write_xlsx(pop.anos,"C:/Users/Nikolas/OneDrive/Documentos/Documentos/Projeto_MADE/pop.anos.xlsx")

###### Renda per capita

gdp <- data.frame( anos ,  renda.1/PIA *1000000 )

names(gdp) <- c("year", "value")


# Gráfico base máxima sobre a renda - MAIOR.BASE.IR / RENDApCAP ________________________________________________________________________________________________

maxbase <- rep(0,length(base[1,]))

for(i in 1:length(base[1,])){
  
  maxbase[i] <- max(base[i][ is.na(base[i]) == FALSE])
  
}


plot(gdp$year[1:96],maxbase[1:96]/gdp$value[1:96], type="l", xlab="Anos", ylab="Relação Base/Renda")


# Criação de sequência de pontos _______________________________________________________________________________________________________________________________

# Vetor usado para calcular renda relativa à renda per capita anual

from <- 0
to <- 18
length.out <- 1801
by <- ((to - from)/(length.out - 1))

points <- seq(from = from, to = to, by = by)

points <- points[5:length(points)]


# Area dos pontos

area.points <- points

area.points[1] <- pnorm(points[1],1,(1-0.05)/2)

for(i in 2:length(points)){
  
  area.points[i] <- pnorm(points[i],1,(1-0.05)/2) - pnorm(points[i-1],1,(1-0.05)/2)
  
}

peso <- area.points * length(points)


# Criação vetores dos índices de gini cálculados

gini_tax <- seq_along(1:length(gdp$year))
gini_pincome <- seq_along(1:length(gdp$year))
tax_income <- seq_along(1:length(gdp$year))

gini_after_tax <- seq_along(1:length(gdp$year))

# Criação da matriz para salvar valores de aliquotas dos pontos selecionados

pontos_selecionados <- c(0.5,1,3,5,10,15)

aliq_pontos <- matrix(NA, length(pontos_selecionados) , length(gdp$year))

gdp$year[50]
gdp$value[43]

valor_base  <- base[51]
aliq_year <- aliq[50+1]



# Looping de geração dos dados - pontos com pesos semelhantes

for(i in 1:length(gdp$year)){ # Entra looping por ano ___________________________________________________________________________________________________--
  
  print( gdp$year[i] )
  
  if( gdp$year[i] < 2023 ){
  
    rel_gdp <- points * gdp$value[i] # Cálculo da renda relativa dado a renda anual
    valor_base  <- base[i] # Valores estão no ano-base, que é sempre 1 ano menor do que o ano de exercício.
    aliq_year <- aliq[i] # Alíquotas do exercício
    
    rel_gdp_after_tax <- rel_gdp
    
    valor_base <- valor_base[ is.na(valor_base) == FALSE] # Tira os NAs do vetor
    aliq_year <- aliq_year[ is.na(aliq_year) == FALSE] # Tira os NAs do vetor
  
  # Cálculo do imposto máximo que pode ser pago por faixa. Em outras palavras, se a renda for maior do que a 2º faixa,
  # deve ser pago imposto igual à : Alíq (1º faixa) * (Base 2º faixa - Base 1º faixa)
  
    totalvalue_range <- ( valor_base[2:length(valor_base)] - valor_base[1:(length(valor_base)-1)] ) * aliq_year[2:(length(aliq_year)-1)]
    totalvalue_range <- c(0,totalvalue_range)
  
  # Criação dos vetores que armazenam informação do tax burden por ponto e % da renda gasta em imposto por ponto
  
    tax_burden <- seq_along(1:500)
    tax_pincome <- seq_along(1:500)
    acc_tax_burden_equal <- seq_along(1:500)
    acc_tax_burnen_obs <- seq_along(1:500)
    acc_tax_pincome_equal <- seq_along(1:500)
    acc_tax_pincome_obs <- seq_along(1:500)

  
  for(y in 1:length(points)){ # Entra no cálculo de looping por ponto no ano ___________________________________________________________________________________________________--
    
      bases <- c(valor_base)
    
      # Alíquota selecionada:
        # Explicação: Define qual alíquota vai utilizar na diferença entre renda observa - primeira.menor.faixa
      
      aliq_selected <- aliq_year[2:length(aliq_year)][ bases < rel_gdp[y] ]
      aliq_selected <- aliq_selected[ length(aliq_selected) ]
      
      if( length(aliq_selected) == 0) aliq_selected <- 0
      
      # Base selecionada p/ cálculo diferença renda base
        # Explicação: define qual a primeira menor faixa que será usada na diferença para o cálculo da segunda parte do imposto
      
      base_selected <- bases[ bases < rel_gdp[y] ]
      base_selected <- base_selected[ length(base_selected) ]
      
      if( length(base_selected) == 0) base_selected <- 0
      
      # Primeira parte do imposto
      
      # Explicação: identifica quais bases são menores que a renda. Isso quer dizer que deve ser pago integralmente os valores entre as faixas 
      # anteriores. Mostra valores da 1º faixa em diante. Se renda for menor do que a primeira faixa, valor será vazio, que é transformado em zero.
      
      pgto_integral_faixas <- totalvalue_range[ bases < rel_gdp[y] ]
  
      if( length(pgto_integral_faixas) == 0) base_selected <- 0
      
      # Cálculo imposto 
      
      first_part <- sum( pgto_integral_faixas ) # Primeira parte, soma do imposto devido entre faixas. 
      
      second_part <-  aliq_selected * ( rel_gdp[y] - base_selected ) # Segunda parte, diferença da renda em relação ao valor da primeira.menor.faixa
      
      tax <- first_part + second_part # Soma das partes 
      
      tax_burden[y] <- tax # Aloca imposto de Y no vetor
      
      rel_gdp_after_tax[y] <- rel_gdp[y] - tax_burden[y]
      
      # Peso em relação à renda 
      
      tax_pincome[y] <- tax/rel_gdp[y]
    
  }
    
    
    ### Cálculo do índice de Gini da renda
    
    
    rendatotal <- sum(rel_gdp_after_tax)
    
    amostra <- length(rel_gdp_after_tax)
    
    renda_individuo <- rendatotal/amostra
    
    histogram3 <- sort(rel_gdp_after_tax)
    
    area_total <- 0
    acumulado_biss <- 0
    acumulado_renda_ind <- 0
    
    area_acima <- rep(0,amostra)
    acumulado_renda_ind <- rep(0,amostra)
    acumulado_biss <- rep(0,amostra)
    
    
    for(z in 1:amostra){
      
      if(z == 1){
        
        acumulado_biss[z] <- renda_individuo
        acumulado_renda_ind[z] <- histogram3[z]
        
      }else{
        
        acumulado_biss[z] <- acumulado_biss[z-1] + renda_individuo
        acumulado_renda_ind[z] <- acumulado_renda_ind[z-1] + histogram3[z]
        
      }
      
      area_acima <- acumulado_biss - acumulado_renda_ind
      
    }
    

    gini_after_tax[i] <- sum(acumulado_biss - acumulado_renda_ind)/sum(acumulado_biss)
    
  
  #plot(tax_burden,type = "l")
  #plot(tax_pincome, type="l")
  
  
  # Soma tax burden total e soma gasto total c/ imposto em %
  
  total_tax_burden <- sum(tax_burden)
  total_percen_income <- sum(tax_pincome)
  
  
  # Cálculos GINI por ano 
  
  equal_frac_tax_burden    <- total_tax_burden/length(points)
  equal_frac_percen_income <- total_percen_income/length(points)
  
  
  acc_tax_burden_equal <- seq_along(1:500)
  acc_tax_burden_obs <- seq_along(1:500)
  acc_tax_pincome_equal <- seq_along(1:500)
  acc_tax_pincome_obs <- seq_along(1:500)
    
  for(y in 1:length(points)){
    
    if(y == 1){
      
      acc_tax_burden_equal[y] <- equal_frac_tax_burden[y]
      acc_tax_burden_obs[y] <- tax_burden[y]
      acc_tax_pincome_equal[y] <- equal_frac_percen_income[y]
      acc_tax_pincome_obs[y] <- tax_pincome[y]
      
      
    }else{
      
      acc_tax_burden_equal[y] <- equal_frac_tax_burden + acc_tax_burden_equal[y - 1]
      acc_tax_burden_obs[y] <- tax_burden[y] + acc_tax_burden_obs[y - 1]
      acc_tax_pincome_equal[y] <- equal_frac_percen_income + acc_tax_pincome_equal[y - 1]
      acc_tax_pincome_obs[y] <- tax_pincome[y] + acc_tax_pincome_obs[y - 1]
      
    }
  }

    # Alíquota paga pelos pontos
      
      vector <- c()
      
      for( x in 1:length(points) ){
        
        if( any( points[x] == pontos_selecionados) == TRUE ){
          
          vector <- c(vector,x)
          
        }
      }
      
      aliq_pontos[,i] <- tax_pincome[ c(vector)]
      
      
    
  }else{  gini_tax[i] <- NA ;  gini_pincome[i] <- NA; tax_income[i] <- NA   }
    
}


# Gráfico no ggplot da % paga de IR

df3 <- data.frame(t(aliq_pontos), gdp$year )*100


df3[ is.na(df3) ] <- NA

df3$gdp.year <- df3$gdp.year/100

colors <- c("0.5" = "red", "1" = "orange" , "3" = "brown" , "5" = "green" , "10" = "blue" , "15" = "purple")
colors2 <- c("0.5" = "#45ff66", "1" = "#EB52FF" , "3" = "#3366FF" , "5" = "#FEFF41" , "10" = "brown" , "15" = "#000000")

# Gráfico sem legenda


ggplot(data=df3) +
  geom_line(aes(gdp.year,X1 ), size = 1 , color = colors2[1]) +
  geom_line(aes(gdp.year,X2 ), size = 1 , color = colors2[2]) +
  geom_line(aes(gdp.year,X3 ), size = 1 , color = colors2[3]) +
  geom_line(aes(gdp.year,X4 ), size = 1, color = colors2[4]) +
  geom_line(aes(gdp.year,X5 ), size = 1, color = colors2[5]) +
  geom_line(aes(gdp.year,X6 ), size = 1, color = colors2[6]) +
  scale_color_manual(values = colors2) +
  labs( y = "Alíquota efetiva do IR" , x = "Ano") + 
  scale_x_continuous(breaks= seq(1925,2022,10)) + 
  theme_bw() +
  theme(legend.position = "top")


# Gráfico com legenda

Legenda <- colors2

ggplot(data=df3, aes(gdp.year, y=X1, col=Legenda)) +
  geom_line(aes(y=X1,col="0.5"), size = 1) +
  geom_line(aes(y=X2,col="1"), size = 1 ) +
  geom_line(aes(y=X3,col="3"), size = 1 ) +
  geom_line(aes(y=X4,col="5"), size = 1 ) +
  geom_line(aes(y=X5,col="10"), size = 1 ) +
  geom_line(aes(y=X6,col="15"), size = 1) +
  scale_color_manual(values = colors2) +
  labs( y = "Alíquota efetiva do IR (em %)" , x = "Ano") + 
  scale_x_continuous(breaks= seq(1925,2022,10)) + 
  theme_bw() +
  theme(legend.position=c(.2,.65),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        text = element_text(size=14)
        
        )


# Fim ! 


