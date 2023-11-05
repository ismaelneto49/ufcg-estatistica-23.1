data <- read.csv("/home/ismael/IdeaProjects/ufcg/besteiras/estatistica/penguin.csv")

# declaração da função que calcula a moda
calcular_moda <- function(column) {
  counts <- table(column)
  mode_value <- names(counts[counts == max(counts)])
  return(mode_value)
}

# atribuição de variáveis para comprimento do bico e massa corporal do pinguin
comprimento_bico <- data$comprimento_bico_mm
massa_corporal <- data$massa_corporal_g

# cálculo das grandezas pedidas na questão
media <- mean(comprimento_bico)
mediana <- median(comprimento_bico)
moda <- calcular_moda(comprimento_bico)
variancia <- var(comprimento_bico)
desvio_padrao <- sqrt(variancia)
coeficiente_correlacao <- cor(comprimento_bico, massa_corporal)

print("    Média: ", media)
print("    Mediana: ", mediana)
print("    Moda: ", moda)
print("    Variância: ", variancia)
print("    Desvio padrão: ", desvio_padrao)
print("    Coeficiente de correlação: ", coeficiente_correlacao)

hist(massa_corporal, breaks = 5, main = "Massa corporal", xlab = "Massa (g)", ylab = "Frequência", col = "lightblue")
boxplot(massa_corporal, main="Massa corporal", horizontal = TRUE, xlab="Massa (g)", col="lightblue")
plot(comprimento_bico, massa_corporal, main="Gráfico de Dispersão", xlab="Comprimento do bico", ylab="Massa corporal", col="lightblue", pch=19)
