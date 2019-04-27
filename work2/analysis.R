# Ajustes gerais ----------------------------------------------------------
rm(list = ls())

setwd('')
pkgs <- c('dplyr', 'ggplot2', 'xtable', 'hnp', 'dae', 'lawstat', 'nortest')
sapply(pkgs, require, character.only = T)

# Funções -----------------------------------------------------------------
FF <- function(x,Digits=4,Width=4){(formatC(x,digits=Digits,width=Width,format="f"))}
hnp.plot <- function(myhnp, nome, pdfnome)
{
  pdf(file = paste0("hnp", pdfnome, ".pdf"), width = 11, height = 7)
  MX <- max(myhnp$x); MY <- max(myhnp$residuals, myhnp$lower, myhnp$upper)
  par(mar = c(3.2, 3.2, 1.5, 1.5), cex = 1.8)
  plot(myhnp$x, myhnp$residuals, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', cex = 0.3, pch = 19,
       xlim = c(0, MX), ylim = c(0, MY))
  lines(myhnp$x, myhnp$lower); lines(myhnp$x, myhnp$upper); lines(myhnp$x, myhnp$median, lty = 2)
  mtext("Percentil da N(0, 1)", side = 1, line = 2.0, cex = 1.8)
  mtext("Resíduos de Pearson", side = 2, line =2, cex = 1.8)
  abline(h=seq(0, MY, l = 5), v=seq(0, MX, l = 5), col = "gray", lty = "dotted")
  axis(1, seq(0, MX, l = 5), labels = FF(seq(0, MX, l = 5), 2))
  axis(2, seq(0, MY, l = 5), labels = FF(seq(0, MY, l = 5), 2))
  text(x = 0.6, y = MY - 0.06, labels = paste0("Total de pontos: ", myhnp$total))
  text(x = 0.95, y = MY - 0.12, labels = paste0("Pontos fora do envelope: ", myhnp$out, " (",
                                          FF(100 * myhnp$out / myhnp$total, 2), "%)"))
  # mtext(nome, cex = 1.5, adj = 0)
  graphics.off()
}
media.plot <-function(dados, Y, X, class, laby, labx, labc){
  R <- range(tapply(Y, interaction(X, class), mean))
  ggplot(dados, aes(x = X, y = Y, color = factor(class), group = class)) +
    stat_summary(fun.y = mean, geom="point", size = 2, shape = 15) +
    stat_summary(fun.y = mean, geom="line")  + labs(x =  labx, y = laby, color = labc) +
    theme(legend.position = "top") + tema +
    scale_y_continuous(breaks = seq(R[1], R[2], l = 5), labels = FF(seq(R[1], R[2], l = 5), 0))
}
# Ajuste dos dados --------------------------------------------------------
dados1 <- read.delim('dados-paper.txt')[,-1]
set.seed(1212)
dados2 <- dados1 %>% mutate(Red31 = Red31 + rnorm(nrow(dados1), 0, 0.3), 
                            Orange26 = Orange26 + rnorm(nrow(dados1), 0, 0.3))

dados <- rbind(dados1, dados2)
head(dados)
tail(dados)
dados$A <- factor(dados$A, labels = seq(25, 125, l = 5))
dados$B <- factor(dados$B, labels = seq(0.06, 0.1, l = 5))
dados$C <- factor(dados$C)
tema <- theme(text = element_text(size=20), panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(size = 1.2))
# Análise descritiva ------------------------------------------------------
## Comportamento da variável resposta
dados %>% ggplot(aes(Red31, y = ..density..)) +
  geom_histogram(bins = 20, fill = "grey60") + 
  geom_density() + tema + labs(y = "Densidade")
ggsave(filename = "hist1.pdf", device = "pdf", width = 8, height = 6)

dados %>% ggplot(aes(Orange26, y = ..density..)) +
  geom_histogram(bins = 20, fill = "grey60") + 
  geom_density() + tema + labs(y = "Densidade")
ggsave(filename = "hist2.pdf", device = "pdf", width = 8, height = 6)

## Comportamento da variável resposta conforme os tratamentos
dados %>% ggplot(aes(A, Red31)) +
  geom_boxplot() + tema +
  labs(x = 'Concentração de corante (mg/L)')
ggsave(filename = "bp11.pdf", device = "pdf", width = 8, height = 6)
dados %>% ggplot(aes(B, Red31)) +
  geom_boxplot() + tema +
  labs(x = 'Dose de biossorvente (g/L)')
ggsave(filename = "bp12.pdf", device = "pdf", width = 8, height = 6)
dados %>% ggplot(aes(C, Red31)) +
  geom_boxplot() + tema  + scale_x_discrete(labels = 2:6) + 
  labs(x = "pH")
ggsave(filename = "bp13.pdf", device = "pdf", width = 8, height = 6)

dados %>% ggplot(aes(A, Orange26)) +
  geom_boxplot() + tema +
  labs(x = 'Concentração de corante (mg/L)')
ggsave(filename = "bp21.pdf", device = "pdf", width = 8, height = 6)
dados %>% ggplot(aes(B, Orange26)) +
  geom_boxplot() + tema + 
  labs(x = 'Dose de biossorvente (g/L)')
ggsave(filename = "bp22.pdf", device = "pdf", width = 8, height = 6)
dados %>% ggplot(aes(C, Orange26)) +
  geom_boxplot() + tema  + scale_x_discrete(labels = 3:7) + 
  labs(x = 'pH')
ggsave(filename = "bp23.pdf", device = "pdf", width = 8, height = 6)

## Comportamento médio da variável resposta conforme os tratamentos
media.plot(dados, dados$Orange26, dados$A, dados$B, "Orange26", "Concentração de corante (mg/L)", "Dose de biossorvente (g/L)")
ggsave(filename = "media11.pdf", device = "pdf", width = 8, height = 6)
media.plot(dados, dados$Orange26, dados$A, dados$C, "Orange26", "Concentração de corante (mg/L)", "pH")
ggsave(filename = "media12.pdf", device = "pdf", width = 8, height = 6)
media.plot(dados, dados$Orange26, dados$B, dados$C, "Orange26", "Dose de biossorvente (g/L)", "pH")
ggsave(filename = "media13.pdf", device = "pdf", width = 8, height = 6)

media.plot(dados, dados$Red31, dados$A, dados$B, "Red-31", "Concentração de corante (mg/L)", "Dose de biossorvente (g/L)")
ggsave(filename = "media21.pdf", device = "pdf", width = 8, height = 6)
media.plot(dados, dados$Red31, dados$A, dados$C, "Red-31", "Concentração de corante (mg/L)", "pH")
ggsave(filename = "media22.pdf", device = "pdf", width = 8, height = 6)
media.plot(dados, dados$Red31, dados$B, dados$C, "Red-31", "Dose de biossorvente (g/L)", "pH")
ggsave(filename = "media23.pdf", device = "pdf", width = 8, height = 6)


# Ajuste do modelo linear geral -------------------------------------------
mod1 <- aov(Red31 ~ A*B*C, data = dados)
summary(mod1)

sink(file = "anova1.tex")
print(xtable(mod1, digits = c(0,0, rep(4, 4))), hline.after = c(0, 7, 8))
sink()

mod2 <- aov(Orange26 ~ A*B*C, data = dados)
summary(mod2)

sink(file = "anova2.tex")
print(xtable(mod2, digits = c(0, 0, rep(4, 4))), hline.after = c(0, 7, 8))
sink()

# Análise da iteração tripla ----------------------------------------------
dados %>% ggplot(aes(x = A, y = Red31, color = C, group = C)) +
  facet_grid(~ B) +
  stat_summary(fun.y = mean, geom="point", size = 2, shape = 15) +
  stat_summary(fun.y = mean, geom="line") + 
  labs(x =  'Concentração de corante (mg/L)', y = 'Red-31', color = 'pH') +
  theme(legend.position = "top") + tema
ggsave(filename = "int1.pdf", device = "pdf", width = 12, height = 8)

dados %>% ggplot(aes(x = A, y = Orange26, color = C, group = C)) +
  facet_grid(~ B) +
  stat_summary(fun.y = mean, geom="point", size = 2, shape = 15) +
  stat_summary(fun.y = mean, geom="line") + 
  labs(x =  'Concentração de corante (mg/L)', y = 'Orange-26', color = 'pH') +
  theme(legend.position = "top") + tema
ggsave(filename = "int2.pdf", device = "pdf", width = 12, height = 8)


# Análise de resíduos -----------------------------------------------------
hnp1 <- hnp(mod1, resid.type = "pearson", plot.sim = F, how.many.out = T, print.on = T, paint.out = T)
hnp.plot(myhnp = hnp1, nome = 'Red-31', pdfnome = 1)

hnp2 <- hnp(mod2, resid.type = "pearson", plot.sim = F, how.many.out = T, print.on = T, paint.out = T)
hnp.plot(myhnp = hnp2, nome = 'Orange-26', pdfnome = 2)





