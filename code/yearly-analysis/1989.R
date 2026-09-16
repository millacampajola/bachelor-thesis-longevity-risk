## bootstrap 1939-1989

LC_fit_1989 <- fit(LC, data = ITA, ages.fit = 0:90, years.fit = 1939:1989)
LC_boot_1989 <- bootstrap(LC_fit_1989, nBoot = 1000, type = "semiparametric")
LC_sim_boot_1989 <- simulate(LC_boot_1989, h = 25)

## ---- estrazione coorte ----

coorte <- 1924 # coorte da estrarre

# anni previsti
anni_forecast <- 1990:(1990+25-1)  # perché h = 25
eta <- 0:90

# seleziono solo combinazioni (età, anno) coerenti con la coorte scelta
indici <- which(outer(anni_forecast, eta, "-") == coorte, arr.ind = TRUE)

# estraggo i tassi simulati per la coorte scelta
rates_coorte <- sapply(1:nrow(indici), function(i) {
  LC_sim_boot_1989$rates[indici[i, 2], indici[i, 1], ]
})

# matrice stessa dim con le probabilità di sopravvivenza t_p_x
survival_matrix <- function(rates_coorte) {
  n_sims  <- nrow(rates_coorte)
  n_years <- ncol(rates_coorte)
  
  surv <- matrix(NA, nrow = n_sims, ncol = n_years) 
  
  for (sim in 1:n_sims) {
    mu <- rates_coorte[sim, ]
    cumhaz <- cumsum(mu)
    surv[sim, ] <- exp(-cumhaz)
  }
  
  return(surv)
}

surv_probs <- survival_matrix(rates_coorte)

## ---- calcolo dei benefici ----

i <- 0.025        # tasso d'interesse      
v <- 1 / (1 + i)
R <- 100          # rata

annuity <- function(v, R, probs){
  a <- 0
  for(i in 1:length(probs)){
    a <- a + probs[i] * v^i * R
  }
  return(a)
}

annuities_1989 <- array(NA, dim = 1000)
for (j in 1:1000){
  annuities_1989[j] <- annuity(v, R, surv_probs[j, ]) 
}

# istogramma
summary_1989 <- summary(annuities_1989)
hist(annuities_1989)
points(mean(annuities_1989), 0, col = "blue", pch = 19, cex = 1.5)
