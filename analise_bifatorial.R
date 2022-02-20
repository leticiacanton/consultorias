dados <- read.table("dados.txt", header = T)   ## lendo um conjunto de dados em txt, header = T tem nome das colunas na 1? linha ##
dados

attach(dados)
## ANÁLISE DESCRITIVA ##

trat <- as.factor(TRAT)       
tempo <- as.factor(TEMPO)  

## resumo descritivo para cada fator ##
tapply(PH, trat, summary)  
tapply(PH, tempo, summary)  


## m?dia do etanol, para cada tratamento ##
mean1 <- tapply(PH, list(trat, tempo), mean)
mean1


## boxplot ##
par(mfrow = c(1,2))  
boxplot(PH ~ trat, xlab = "Tratamento", ylab = "PH") 
boxplot(PH ~ tempo, xlab = "Tempo", ylab = "PH") 

## GR?FICOS DE INTERA??O ##

par(mfrow = c(1,2)) 
interaction.plot(trat, tempo, PH)
interaction.plot(tempo, trat, PH)


## NO PACOTE ExpDes ##

require(ExpDes)
fat2.crd(trat, tempo, PH, quali = c(TRUE, TRUE), 
mcomp = "tukey", fac.names = c("Tratamento", "Tempo"), sigT = 0.05, sigF = 0.05)

