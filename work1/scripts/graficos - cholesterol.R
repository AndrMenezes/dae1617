library(ggplot2)
library(agricolae)
data("cholesterol")
?cholesterol
names(cholesterol)

# Histograma da variável resposta -----------------------------------------

hist <- ggplot(data = cholesterol, aes(response))
hist <- hist + geom_histogram(bins=10, col="black", fill="slategray")
hist <- hist + labs(x = "Colesterol mg/dl", y = "Frequência") 
hist


# Comportamento da variável resposta conforme o tratamento ----------------
bp <- ggplot(data = cholesterol, aes(as.factor(trt), response))
bp <- bp +  stat_boxplot(geom ='errorbar') 
bp <- bp + geom_boxplot(fill = "slategray")
bp <- bp + labs(y = "Colesterol mg/dl", x = "Tratamento")
bp

p1 <- ggplot(data = cholesterol, 
             aes(x=1:length(cholesterol$trt), y=response, col=as.factor(trt)))
p1 <- p1 + geom_point(cex = 3)
p1 <- p1 + labs(x = "Índice", y = "Colesterol mg/dl", col="Tratamento")
p1

setwd("C:/Users/André Felipe/Dropbox/UEM/3° Série/Planejamento e Análise de Experimentos I/Trabalho - Testes (Duncan Bonferoni e Dunnet)/Apresentação")
ggsave(filename = "hist.pdf", plot = hist, device = "pdf", scale = 1.4)
ggsave(filename = "boxplot.pdf", plot = bp, device = "pdf", scale = 1.4)
ggsave(filename = "plot.pdf", plot = p1, device = "pdf", scale = 1.4)
