dummy <- data.frame(X = 0:10, Y = 3:13)
ggplot(dummy, aes(X, Y)) +
  geom_line() +
  labs(x = "X (predictora)", y = "Y (respuesta)") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15), breaks = NULL) +
  scale_x_continuous(expand = c(0, 0.05), limits = c(0, 10), breaks = NULL) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 22)
  ) +
  annotate("text", x = 1, y = 2, label = "paste(beta[0], ' (intercepto)')", hjust = 0, parse = T, size = 8) +
  annotate('segment', x = 0.1, xend = 0.8, y = 3, yend = 2) + 
  annotate("text", x = 6.25, y = 8.25, label = "paste(beta[1], ' (pendiente)')", hjust = 0, parse = T, size = 8) +
  annotate("text", x = 5.5, y = 7.5, label = "1", hjust = 0, parse = T, size = 8) +
  annotate('segment', x = 5, xend = 6, y = 8, yend = 8) + 
  annotate('segment', x = 6, xend = 6, y = 8, yend = 9) + 
  annotate("point", x = 4, y = 10, colour = "red", size = 1.5) +
  annotate('segment', x = 4, xend = 4, y = 7, yend = 10) + 
  annotate("text", x = 3.5, y = 8.5, label = "paste(italic(e)[i])", hjust = 0, parse = T, size = 8) +
  annotate("text", x = 4, y = 10.5, label = "paste(italic(X)[i]~italic(Y)[i])", hjust = 0, parse = T, size = 8)

ggsave(last_plot(), file="fig_3/slr.png", w=7, h=4)

#fig2

## Sample data
set.seed(0)
x <- 0:5
y <- 3 + 1.3 * x + rnorm(6, 0, 0.75)
dat <- data.frame(x, y)

## breaks: where you want to compute densities
dat$section <- as.factor(0:5)

## Get the residuals
m <- lm(y ~ x, data=dat)
sigma <- summary(m)$sigma
dat$pred <- predict(m)

# Calcular densidadas
dens <- do.call(rbind, lapply(split(dat, dat$section), function(x) {
  xs <- seq(x$pred - 3*sigma, x$pred + 3*sigma, len = 50)
  res <- data.frame(y = xs + 0.5, x = dnorm(xs, x$pred, sigma))
}))
dens$section <- rep(levels(dat$section), each = 50)

## Just normal
ggplot(dat, aes(x, y)) +
  geom_point(size = 2) +
  geom_smooth(method="lm", se = F, color = "black") +
  geom_path(data = dens, aes(x + rep(0:5, each = 50), y, group = section), color = "gray", lwd = 1.1) +
  theme_bw() + tema +
  geom_vline(xintercept = 0:5, lty = 2,  lwd = 0.25) +
  xlim(0, 6) +
  labs(x = "X", y = "Y")

ggsave(last_plot(), file="fig_3/slr_assump.png", w=7, h=4)
