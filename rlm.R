require(rsq)

## com todas as vari?veis
summary(glm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12))

model1 = glm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12)
rsq(model1)

## s? com as significativas
summary(glm(y~x1+x2+x3+x4+x5+x6+x7+x12))

model2=glm(y~x1+x2+x3+x4+x5+x6+x7+x12)

rsq(model2)
