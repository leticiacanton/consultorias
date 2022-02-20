dados=read.table("mbi2.txt",h=T)

require(tidyverse)
require(knitr)
require(kableExtra)
require(ggplot2)
require(lavaan)
require(semPlot)

## boxplot

ggplot(dados,aes(x=item,y=escala))+
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=90, hjust=1))

## analise fatorial confirmatoria

dados2=read.table("mbi.txt",h=T)
dados2


modelo_afc='eficacia =~ eficaz + participativo + bom + estimulado
+aprendeu + acompanhou
exaustao =~ exausto + arrastando + cansado + tenso + esgotado
descrenca =~ desinteressado + desentusiasmado + cetico + duvidas'

afc_dados=cfa(modelo_afc,data = dados2,ordered = names(dados2))
afc_dados              
              

summary(afc_dados, standardized=T)


# correlação

# cov (x,y)/dp(x)*dp(y) 
# cov é dada
# dp pega a raiz quadrada da var (que é dada)


fitMeasures(afc_dados,c("chisq","df","pvalue","cfi","tli","rmsea"))

semPaths(afc_dados)

## alpha de cronbach

require(psych)

fator_ef=select(dados2,eficaz, participativo, bom, estimulado,
                aprendeu, acompanhou)
fator_ex=select(dados2, exausto, arrastando, cansado, tenso, esgotado)
fator_des=select(dados2, desinteressado, desentusiasmado, cetico, duvidas)



alpha_ex = alpha(fator_ex)
alpha_ex$total

alpha_des = alpha(fator_des)
alpha_des$total

alpha_ef = alpha(fator_ef)
alpha_ef$total

## AVE - average variance extracted

require(semTools)

reliability(afc_dados) ## essa função da o alpha tbm



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
  









