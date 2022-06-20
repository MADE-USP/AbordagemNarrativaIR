### Código para interpolar % da pop como PIA usando os censos demográficos

# Donwload do pacote qu contem a função "spline"
install.packages("stats")

#  Carregamento do pacote
library(stats)

# % da população em idade ativa. Censos de 1900 até 2010

x <- c(55.1 ,	57.09	,57.43,	58,	57.24,	57.79,	61.68,	65.29,	70.41,	75.91) # Porcentagens

num <- 2010 - 1900 # Anos


# Função SPLINE sobre para interpolar o % da pop que consistia em PIA

resultado <- spline(x , n=110)

length(resultado$y)

plot(x)
plot(resultado$y, type="l")

# Intervalo para selecionar:

num2 <- 1924 - 1900

percentage.pia <- resultado$y[num2:110] # Resultado final: série da % da pop. com idade ativa de 1924 até 2010

