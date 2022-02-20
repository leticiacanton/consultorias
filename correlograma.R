rm(list = ls())

library("raster")
library("dismo")
library("rgdal")
library("corrplot")
library("RStoolbox")
library("vegan")
library("psych")

data=read.table("clipboard", h=T, sep="", dec=",")
attach(data)
names(data)

#melissa=read.csv2(file.choose(),h=T,sep=";",dec=",")
#data=data[,-1]

#attach(melissa)
# tabela da correlacao
cor <- cor(data)
cor
round(cor, 2) # arredondamento dos valores para dois valores decimais
# exportar tabela com a correlacao
write.table(round(cor(data), 2), "cor.xls", row.names = T, sep = "\t")
write.table(ifelse(cor(data) >= 0.7, "Sim", "Não"), "cor_afirmacao.xls",
            row.names = T, sep = "\t")
# plot da correlacao
tiff("cor_ma.tif", width = 18, height = 18, units = "cm", res = 300, compression =
       "lzw")

corrplot(cor(data), type = "lower", diag = F, tl.srt = 45, mar = c(3, 0.5, 2, 1),
         title = "EVA e WHOQOL-BREF")
dev.off()

