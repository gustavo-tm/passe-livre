library(ggthemes)
library(ggplot2)
windowsFonts(A = windowsFont("Times New Roman"))

pontos <- seq(0, 1, 0.001)[-1]

gg <- ggplot() +
  ylim(c(0, 120)) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        text = element_text(family = "A")) +
  labs(y = "E(U)",
       x = "Comparecimento")

plot.1 <- gg +
  geom_line(aes(pontos, 7 / pontos + 5)) +
  geom_hline(yintercept = 15) +
  geom_vline(xintercept = 7/(15-5), linetype = "dashed", alpha = .3) +
  annotate("text", x=.25, y= 15+2^2, label="CMG") +
  annotate("text", x=.25, y= 40+2^2, label="BMG") +
  labs(title = "Mercado de votos")

plot.2 <- gg +
  geom_line(aes(pontos, 7 / pontos + 5)) +
  geom_hline(yintercept = 15, linetype = "dashed") +
  geom_hline(yintercept = 20) +
  geom_vline(xintercept = 7/(15-5), linetype = "dashed", alpha = .3) +
  geom_vline(xintercept = 7/(20-5), linetype = "dashed", alpha = .3) +
  annotate("text", x=.25, y= 20+2^2, label="CMG'") +
  annotate("text", x=.25, y= 40+2^2, label="BMG") +
  labs(title = "Mercado de votos - Mudança I")

plot.3 <- gg +
  geom_line(aes(pontos, 7 / pontos + 5), linetype = "dashed") +
  geom_line(aes(pontos, 9 / pontos + 5)) +
  geom_hline(yintercept = 15) +
  geom_vline(xintercept = 7/(15-5), linetype = "dashed", alpha = .3) +
  geom_vline(xintercept = 9/(15-5), linetype = "dashed", alpha = .3) +
  annotate("text", x=.25, y= 15+2^2, label="CMG") +
  annotate("text", x=.25, y= 50+2^2, label="BMG'") +
  labs(title = "Mercado de votos - Mudança II")

plot.4 <- gg +
  geom_line(aes(pontos, 7 / pontos + 5), linetype = "dashed") +
  geom_line(aes(pontos, 7 / pontos + 7)) +
  geom_hline(yintercept = 15) +
  geom_vline(xintercept = 7/(15-5), linetype = "dashed", alpha = .3) +
  geom_vline(xintercept = 7/(15-7), linetype = "dashed", alpha = .3) +
  annotate("text", x=.25, y= 15+2^2, label="CMG") +
  annotate("text", x=.25, y= 50+2^2, label="BMG'") +
  labs(title = "Mercado de votos - Mudança III")

ggsave("output/mercado_votos.png", plot.1,  dpi = 600)
ggsave("output/mercado_votos_I.png", plot.2,  dpi = 600)
ggsave("output/mercado_votos_II.png", plot.3,  dpi = 600)
ggsave("output/mercado_votos_III.png", plot.4,  dpi = 600)

