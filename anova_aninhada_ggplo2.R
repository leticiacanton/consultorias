
## Pacotes ---------------------------------------------------------------------
library(tidyverse)
library(readxl)
require(nlme)
library(lme4)
library(performance)
library(DHARMa)
library(bbmle)
library(car)
library(emmeans)
library(sjPlot)
library(glmmTMB)

## Dados -----------------------------------------------------------------------
dado <- read_excel("planilha.xlsx", 
                   col_types = c("text", "numeric", "text", 
                                 "text", "numeric", "numeric", "numeric", 
                                 "numeric"))
dado
names(dado)

## Gráficos --------------------------------------------------------------------
dado %>%
  ggplot(aes(x = Semana, y = n, group = Semana)) +
  geom_boxplot(aes(fill = setor)) + facet_grid (. ~ setor) + 
  labs(y = "nº de cascudinhos (Alphitobius diaperinus)") +
  theme_bw()

ggsave("semana x area.svg", he = 15, wi = 20, un = "cm", dpi = 600)

  
ggplot(data = dado, aes(x = Semana, y = n, group = armadilha)) + 
  geom_line(aes(colour = armadilha)) + facet_grid (. ~ setor) + 
  labs(y = "n cascudinho") +
  theme_bw()

ggplot(data = dado, aes(x = Semana, y = n, colour = setor, group = setor)) +
  geom_smooth(se = TRUE) + labs(y = "n cascudinho")

droga_wide <- dado %>% select(setor, armadilha, Semana, n) %>% spread(Semana, n)
droga_wide

tiempos <- droga_wide[3:6]

GGally::ggpairs(tiempos) 

cor(tiempos) %>% 
  corrplot::corrplot(type = "upper",
                     method = "number")

names(dado)

## Modelos ---------------------------------------------------------------------

# 1 - Mod. Linear
ml <- lm(n ~ Semana + setor + 1|armadilha, data = dado)
ml
summary(ml)
## AIC: 1230.286

# Pressupostos
rpml = resid(ml, type="pearson") 
shapiro.test(rpml)
predml = fitted(ml, type ="response")

plot(predml, rpml, xlab ="predichos", ylab= "Residuos de pearson", main = "Gráfico de RP vs PRED", cex.main=.8)
abline(0,0) # formato de cone, o que não é bom

boxplot(rpml ~ as.factor(Semana), data = dado, 
        xlab = "Semana", 
        ylab = "res", 
        cex.lab = 1.5) 
abline(h = 0, lty = 2) # semana 4 com variação muito grande 

boxplot(rpml ~ as.factor(setor), data = dado, 
        xlab = "Setor", 
        ylab = "n", 
        cex.lab = 1.5) 
abline(h = 0, lty = 2) # Setor 2 com variação maior de valores


# 2 - Mod. Poisson
mp <- glmmTMB(n ~ as.factor(Semana) + setor + (1|armadilha), data = dado, family = poisson) 
mp
summary(mp)
## AIC: 2209,6

# Pressupostos
rpmp = resid(mp, type="pearson") 
shapiro.test(rpmp)
predmp = fitted(mp, type ="response")

plot(predmp, rpmp, xlab ="predichos", ylab= "Residuos de pearson", main = "Gráfico de RP vs PRED", cex.main=.8)
abline(0,0) # Alguns valores ainda muito dispersos 

boxplot(rpmp ~ as.factor(Semana), data = dado, 
        xlab = "Semana", 
        ylab = "res", 
        cex.lab = 1.5) 
abline(h = 0, lty = 2) # relativo as semanas não está tão preocupante

boxplot(rpmp ~ as.factor(setor), data = dado, 
        xlab = "Setor", 
        ylab = "res", 
        cex.lab = 1.5) 
abline(h = 0, lty = 2) # relativo ao setor não está tão preocupante

# Chegar sobre ou subdispersão
check_overdispersion(mp) ## Overdispersion detected.
somp  <- simulateResiduals(fittedModel = mp, plot = T, n = 999) 
testDispersion(somp)

# 3 - Binomial negativo
mbn <- glmmTMB(n ~ as.factor(Semana) + setor + (1|armadilha), data = dado, family="nbinom2") 
summary(mbn)
### AIC: 941.9

# Pressupostos
rpmbn = resid(mbn, type="pearson") 
shapiro.test(rpmbn)
predmbn = fitted(mbn, type ="response")

plot(predmbn, rpmbn, xlab ="predichos", ylab= "Residuos de pearson", main = "Gráfico de RP vs PRED", cex.main=.8)
abline(0,0) # Alguns valores ainda muito dispersos 

boxplot(rpmbn ~ as.factor(Semana), data = dado, 
        xlab = "Semana", 
        ylab = "res", 
        cex.lab = 1.5) 
abline(h = 0, lty = 2) # relativo as semanas não está tão preocupante

boxplot(rpmbn ~ as.factor(setor), data = dado, 
        xlab = "Setor", 
        ylab = "res", 
        cex.lab = 1.5) 
abline(h = 0, lty = 2) # relativo ao setor não está tão preocupante

# Chegar sobre ou subdispersão
sombn  <- simulateResiduals(fittedModel = mbn, plot = T, n = 999) 
xtestDispersion(sombn)

## 4 - Compoisson
mcp <- glmmTMB(n ~ as.factor(Semana) + setor + (1|armadilha), data = dado, family = "compois") 
summary(mcp) ## 939.4

# Pressupostos
rpmcp = resid(mcp, type="pearson") 
shapiro.test(rpmcp)
predmcp = fitted(mcp, type ="response")

plot(predmcp, rpmcp, xlab ="predichos", ylab= "Residuos de pearson", main = "Gráfico de RP vs PRED", cex.main=.8)
abline(0,0) # Alguns valores ainda muito dispersos 

boxplot(rpmcp ~ as.factor(Semana), data = dado, 
        xlab = "Semana", 
        ylab = "res", 
        cex.lab = 1.5) 
abline(h = 0, lty = 2) # relativo as semanas não está tão preocupante

boxplot(rpmcp ~ as.factor(setor), data = dado, 
        xlab = "Setor", 
        ylab = "res", 
        cex.lab = 1.5) 
abline(h = 0, lty = 2) # relativo ao setor não está tão preocupante

# Chegar sobre ou subdispersão
somcp  <- simulateResiduals(fittedModel = mcp, plot = T, n = 50) 
testDispersion(somcp)

## 5 - GLS
mgls <- gls(n ~ as.factor(Semana) + setor, data = dado, 
            weights=varIdent(form=~1|as.factor(Semana)))

summary(mgls)

rgls = residuals(mgls, type="pearson") #guardamos los residuos de Pearson = estandarizados
predgls = fitted(mgls)
plot(predgls, rgls, xlab="Predichos", ylab="Residuos estandarizados",main="Gráfico de dispersión de RE vs PRED" )
abline(0,0)
shapiro.test(rgls)

## 6 - Tweedie
mtw <- glmmTMB(n ~ as.factor(Semana) + setor + (1|armadilha), data = dado, family = "tweedie") 
summary(mtw) ## 939.4

# Pressupostos
rpmtw = resid(mtw) 
shapiro.test(rpmtw)
predmtw = fitted(mtw, type ="response")

plot(predmtw, rpmtw, xlab ="predichos", ylab= "Residuos de pearson", main = "Gráfico de RP vs PRED", cex.main=.8)
abline(0,0) # Alguns valores ainda muito dispersos 

boxplot(rpmtw ~ as.factor(Semana), data = dado, 
        xlab = "Semana", 
        ylab = "res", 
        cex.lab = 1.5) 
abline(h = 0, lty = 2) # relativo as semanas não está tão preocupante

boxplot(rpmtw ~ as.factor(setor), data = dado, 
        xlab = "Setor", 
        ylab = "res", 
        cex.lab = 1.5) 
abline(h = 0, lty = 2) # relativo ao setor não está tão preocupante

################################################################################

## Subdispersão
mp2 <- glmmTMB(n ~ as.factor(Semana) + setor + (1|armadilha), ziformula = ~as.factor(Semana) + setor, data = dado, family = poisson) 
mbn2 <- glmmTMB(n ~ as.factor(Semana) + setor + (1|armadilha), ziformula = ~as.factor(Semana) + setor, data = dado, family="nbinom2") 
mtw2 <- glmmTMB(n ~ as.factor(Semana) + setor + (1|armadilha), ziformula = ~as.factor(Semana) + setor, data = dado, family = "tweedie") 

sommp2 <- simulateResiduals(fittedModel = mp2, plot = T, n = 50) 
testDispersion(sommp2)

sommbn2 <- simulateResiduals(fittedModel = mbn2, plot = T, n = 50) 
testDispersion(sommbn2)

sommtw2 <- simulateResiduals(fittedModel = mtw2, plot = T, n = 50) 
testDispersion(mtw2) ## !!

#
rpmtw2 = resid(sommbn2) 
shapiro.test(rpmtw2)
predmtw2 = sommbn2$fittedPredictedResponse

plot(predmtw2, rpmtw2, xlab ="predichos", ylab= "Residuos de pearson", main = "Gráfico de RP vs PRED", cex.main=.8)
abline(0,0) # Alguns valores ainda muito dispersos 

boxplot(rpmtw ~ as.factor(Semana), data = dado, 
        xlab = "Semana", 
        ylab = "res", 
        cex.lab = 1.5) 
abline(h = 0, lty = 2) # relativo as semanas não está tão preocupante

boxplot(rpmtw ~ as.factor(setor), data = dado, 
        xlab = "Setor", 
        ylab = "res", 
        cex.lab = 1.5) 
abline(h = 0, lty = 2) # relativo ao setor não está tão preocupante

