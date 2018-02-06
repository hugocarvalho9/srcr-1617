library(neuralnet)
library(hydroGOF)

dados <- read.csv("C:\\Users\\hugoc\\Dropbox\\Trabalho SRCR\\3\\exaustao.csv", header=TRUE, sep=",", dec=".")

#"baralhar" as linhas lidas
dados <- dados[sample(nrow(dados)), ]

dados$FatigueLevel[dados$FatigueLevel = 1 ] <- 1
dados$FatigueLevel[dados$FatigueLevel = 2 ] <- 2
dados$FatigueLevel[dados$FatigueLevel = 3 ] <- 3
dados$FatigueLevel[dados$FatigueLevel >= 4] <- 4

# formula da fadiga 
formulaFadiga <- FatigueLevel ~ Performance.KDTMean +  Performance.MAMean + Performance.DDCMean + Performance.Task

# treino e testes
treino <- dados[1:700, ]
teste  <- dados[701:844,]

# treinar a rede neuronal
resultado1 <- neuralnet(formulaFadiga, treino, hidden=c(6,4), lifesign = "full", linear.output = TRUE, threshold = 0.1)

# plot(resultado1)

#subset para treino fadiga
teste.01 <- subset(teste, select = c("Performance.MAMean","Performance.KDTMean", "Performance.DDCMean","Performance.Task"))

# testar a rede com os novos casos de treino
resultado1.resultados <- compute(resultado1, teste.01)

# imprimir resultados
resultadosFadiga <- data.frame(atual = teste$FatigueLevel, previsao=resultado1.resultados$net.result)
resultadosFadiga$previsao <- round(resultadosFadiga$previsao,digits=0)
resultadosFadiga

# erro
rmse(c(teste$FatigueLevel), c(resultadosFadiga$previsao))