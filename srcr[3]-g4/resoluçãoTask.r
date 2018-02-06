library(neuralnet)
library(hydroGOF)

dados <- read.csv("C:\\Users\\hugoc\\Dropbox\\Trabalho SRCR\\3\\exaustao.csv", header=TRUE, sep=",", dec=".")

#"baralhar" as linhas lidas
dados <- dados[sample(nrow(dados)), ]

# formula da tarefa em execução 
formulaPerformanceTask <- Performance.Task ~ Performance.KDTMean + Performance.DDCMean + Performance.DMSMean +
                                             Performance.AEDMean + Performance.ADMSLMean + FatigueLevel
# treino e testes
treino <- dados[1:700, ]
teste  <- dados[701:844,]

# treinar a rede neuronal
resultado1 <- neuralnet(formulaPerformanceTask, treino, hidden=c(6,4), lifesign = "full", linear.output = TRUE, threshold = 0.1)

#subset para tarefa em execução
teste.01 <- subset(teste, select = c("Performance.KDTMean", "Performance.DDCMean","Performance.DMSMean",
                                     "Performance.AEDMean", "Performance.ADMSLMean", "FatigueLevel"))

# testar a rede com os novos casos de treino
resultado1.resultados <- compute(resultado1, teste.01)

# imprimir resultados
resultadosTask <- data.frame(atual = teste$FatigueLevel, previsao=resultado1.resultados$net.result)
resultadosTask$previsao <- round(resultadosTask$previsao,digits=0)
resultadosTask

# erro
rmse(c(teste$FatigueLevel), c(resultadosTask$previsao))
