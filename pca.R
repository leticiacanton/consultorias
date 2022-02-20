# OBS: NA influencia na PCA, deixar como célula vazia e nao como outra categoria

### memory
rm(list = ls())

### packages
library(vegan)
library(ExpDes.pt)
library(ggplot2)
library(ggfortify)
library(Rmisc)
library(psych)
library(factoextra)
library(FactoMineR)
library(dplyr)
library(dunn.test)



### archive chemistry

#fat=read.table("janaina.txt",h=T)

fat=read.csv2("janaina_gruposnovos3.csv",h=T,sep=";",dec=".",stringsAsFactors=T)
dim(fat)

#levels(fat$grupo)=c('exposto','não exposto')
print(fat)

summary(fat)

# pca mulheres

f=fat[,-c(1:2)]
KMO(f)
pca.c <- FactoMineR::PCA(f, scale.unit = TRUE, graph = F)
pca.c$eig ## autovalores e % de explicação de cada dim

# eigenvalues
factoextra::get_eig(pca.c) %>%
  round(2)

# eigenvalues plot
setwd("..")
dir.create("01_graphs")
setwd("01_graphs")

factoextra::fviz_eig(pca.c, addlabels = TRUE, ylim = c(0, 20), ggtheme = theme_classic())
ggsave("00_screeplot.tiff", he = 15, wi = 20, un = "cm", dpi = 300)

# contributions
pca.c$var$contrib
pca.c$var$coord
pca.c$ind$coord
scores.c=data.frame(pca.c$ind$coord, fat$Grupos)


# selection
pca.c$var$contrib %>%
  tibble::as_tibble() %>%
  dplyr::mutate(var = rownames(pca.c$var$contrib)) %>%
  dplyr::select(var, Dim.1, Dim.2) %>%
  dplyr::arrange(desc(Dim.1))

# contributions
a=factoextra::fviz_contrib(pca.c, choice = "var", axes = 1, ggtheme = theme_classic())
a
a$data

## pedir a linha de cima $data da pra ver quem fica na dim 1 e na dim 2
## tem que passar a linha vermelha no graf abaixo
ggsave("01_contributions_pc1.tiff", he = 15, wi = 20, un = "cm", dpi = 300)

b=factoextra::fviz_contrib(pca.c, choice = "var", axes = 2, ggtheme = theme_classic())
b
b$data
## pedir a linha de cima $data da pra ver quem fica na dim 1 e na dim 2
## tem que passar a linha vermelha no graf abaixo
ggsave("02_contributions_pc2.tiff", he = 15, wi = 20, un = "cm", dpi = 300)

getwd()
# biplot
fviz_pca_biplot(pca.c, label="var", habillage=factor(fat$Grupos),
             addEllipses=T, ellipse.level=0.95, palette = "Dark2",
             title="")
ggsave("05_biplot_pca.png", he = 15, wi = 20, un = "cm", dpi = 300)

## para editar no inkscape salvar como .svg

pca.c$var
## ver quem é cor + e -
#all=fviz_pca_biplot(pca.c, label="var", col.var = "black", 
 #               addEllipses=F, ellipse.level=0.95, palette = "grey",
  #              title="")+theme_classic()
#ggsave("05_biplot_pca_.png", he = 15, wi = 20, un = "cm", dpi = 300)


### Analysis
dic(scores.c$fat.Grupos, scores.c$Dim.1, mcomp="lsd",  sigF=0.05,sigT = 0.05)
dic(scores.c$fat.Grupos, scores.c$Dim.2, mcomp="lsd",  sigF=0.05,sigT = 0.05)
names(scores.c)

tgc <- summarySE(scores.c, measurevar="Dim.1", group="fat.Grupos")
tgc
tgc1 <- summarySE(scores.c , measurevar="Dim.2", group="fat.Grupos")
tgc1

# Use dose as a factor rather than numeric
tgc2 <- tgc
tgc2$fat.grupo <- factor(tgc2$fat.Grupos)

tgc3 <- tgc1
tgc3$fat.grupo <- factor(tgc3$fat.Grupos)


# Error bars represent standard error of the mean - Dim 1
d1=ggplot(tgc2, aes(x=fat.Grupos, y=Dim.1, fill=Dim.1)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Dim.1-se, ymax=Dim.1+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+theme_classic()+xlab("Grupos")
d1
ggsave("04_barplot_pca.Dim1.png", he = 15, wi = 20, un = "cm", dpi = 600)

# Error bars represent standard error of the mean - Dim 2
d2=ggplot(tgc3, aes(x=fat.Grupos, y=Dim.2, fill=Dim.2)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Dim.2-se, ymax=Dim.2+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+theme_classic()+xlab("Grupos")
d2
ggsave("05_barplot_pca.Dim2.png", he = 15, wi = 20, un = "cm", dpi = 300)

library(ggpubr)
  ggarrange(d1, d2, ncol = 2, labels = c("A", "B")) 
ggsave("06_all_graphs.png", he = 10, wi = 20, un = "cm", dpi = 600)

