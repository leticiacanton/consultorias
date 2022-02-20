dados=read.table("mbi2.txt",h=T)

require(tidyverse)
require(knitr)
require(kableExtra)
require(ggplot2)
require(lavaan)
require(semPlot)


## ANOVA two-way

anova=read.table("anova.txt",h=T)
anova

attach(anova)

an=aov(exaustao~as.factor(sexo)*as.factor(periodo),data=anova)

res=an$residuals

hist(res)

shapiro.test(res)

require(car)

homo=leveneTest(exaustao~as.factor(sexo)*as.factor(periodo),
                 data=anova)
homo

summary(an)

TukeyHSD(an)

## kruskal-wallis para os que não passaram (faz para cada fator em separado, não tem a interação entre eles)


desc_sexo=kruskal.test(descrenca~sexo, data=anova)
desc_sexo

desc_per=kruskal.test(descrenca~periodo, data=anova)
desc_per

efic_sexo=kruskal.test(eficacia~sexo, data=anova)
efic_sexo

efic_per=kruskal.test(eficacia~periodo, data=anova)
efic_per
  









