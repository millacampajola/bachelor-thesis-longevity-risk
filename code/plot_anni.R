# plot anni - valore benefici

# supponiamo che tu abbia una lista con i risultati
anni <- seq(1984, 2019, 5)   # gli anni di fitting
n_anni <- length(anni)

# ad esempio hai salvato i summary in una lista
summaries <- list(
  summary_1984, summary_1989, summary_1994,
  summary_1999, summary_2004, summary_2009,
  summary_2014, summary_2019
)

# trasformo in data.frame lungo
df <- data.frame(
  anno   = rep(anni, each = 6),
  misura = rep(c("Min", "Q1", "Mediana", "Mean", "Q3", "Max"), times = n_anni),
  valore = unlist(summaries)
)

# grafico con ggplot
library(ggplot2)

ggplot(df, aes(x = anno, y = valore, color = misura)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Evoluzione distribuzione dei benefici attesi",
       x = "Anno di fitting",
       y = "Valore rendita attuariale",
       color = "Statistica")

summaries_mat <- cbind(
  summary_1984,
  summary_1989,
  summary_1994,
  summary_1999,
  summary_2004,
  summary_2009,
  summary_2014,
  summary_2019_b
)

# righe = Min, 1st Qu., Median, Mean, 3rd Qu., Max
# colonne = anni
colnames(summaries_mat) <- anni

# faccio un plot multiplo con matplot
matplot(anni, t(summaries_mat), type = "l",
        col = c("green3","deepskyblue","white","gold","magenta","darkorange"), 
        lwd = 1.5, lty = 1, xlab = "year", ylab = "annuity value", 
        main = " ", xaxt = "n")

axis(1, at = anni, labels = anni)

legend("topleft", 
       legend = c("Min","1st Qu","Mean","3rd Qu","Max"),
       col = c("green3","deepskyblue","gold","magenta","darkorange"), 
       lty = 1, lwd = 1.5)

points(anni,summaries_mat[1,], col ="green3", pch = 20)
points(anni,summaries_mat[2,], col ="deepskyblue", pch = 20)
points(anni,summaries_mat[4,], col ="gold", pch = 20)
points(anni,summaries_mat[5,], col ="magenta", pch = 20)
points(anni,summaries_mat[6,], col ="darkorange", pch = 20)



