library(ggplot2)
library(agricolae)
data("cholesterol")
?cholesterol
names(cholesterol)

# Histograma da vari�vel resposta -----------------------------------------

hist <- ggplot(data = cholesterol, aes(response))
hist <- hist + geom_histogram(bins=10, col="black", fill="slategray")
hist <- hist + labs(x = "Colesterol mg/dl", y = "Frequ�ncia") 
hist


# Comportamento da vari�vel resposta conforme o tratamento ----------------
bp <- ggplot(data = cholesterol, aes(as.factor(trt), response))
bp <- bp +  stat_boxplot(geom ='errorbar') 
bp <- bp + geom_boxplot(fill = "slategray")
bp <- bp + labs(y = "Colesterol mg/dl", x = "Tratamento")
bp

p1 <- ggplot(data = cholesterol, 
             aes(x=1:length(cholesterol$trt), y=response, col=as.factor(trt)))
p1 <- p1 + geom_point(cex = 3)
p1 <- p1 + labs(x = "�ndice", y = "Colesterol mg/dl", col="Tratamento")
p1

setwd("C:/Users/Andr� Felipe/Dropbox/UEM/3� S�rie/Planejamento e An�lise de Experimentos I/Trabalho - Testes (Duncan Bonferoni e Dunnet)/Apresenta��o")
ggsave(filename = "hist.pdf", plot = hist, device = "pdf", scale = 1.4)
ggsave(filename = "boxplot.pdf", plot = bp, device = "pdf", scale = 1.4)
ggsave(filename = "plot.pdf", plot = p1, device = "pdf", scale = 1.4)
