data <- read.csv("/home/ismael/IdeaProjects/ufcg/besteiras/estatistica/penguin.csv")

calcular_moda <- function(column) {
  counts <- table(column)
  mode_value <- names(counts[counts == max(counts)])
  return(mode_value)
}

comprimento_bico <- data$comprimento_bico_mm
massa_corporal <- data$massa_corporal_g

media <- mean(comprimento_bico)
mediana <- median(comprimento_bico)
moda <- calcular_moda(comprimento_bico)
variancia <- var(comprimento_bico)
desvio_padrao <- sqrt(variancia)
coeficiente_correlacao <- cor(comprimento_bico, massa_corporal)

histograma <- hist(massa_corporal, breaks = 5, main = "Massa corporal", xlab = "Massa (g)", ylab = "Frequência", col = "lightblue")
boxplot(massa_corporal, main="Massa corporal", horizontal = TRUE, xlab="Massa (g)", col="lightblue")
plot(comprimento_bico, massa_corporal, main="Gráfico de Dispersão", xlab="Comprimento do bico", ylab="Massa corporal", col="lightblue", pch=19)
