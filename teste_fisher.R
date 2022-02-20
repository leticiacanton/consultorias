require(stats)

teste = rbind(c(33,31,3),c(23,19,7))

quiquad =  chisq.test(teste)
quiquad

fisher.test(teste, hybrid=TRUE)
