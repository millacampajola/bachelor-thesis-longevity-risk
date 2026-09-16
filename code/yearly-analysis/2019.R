## bootstrap 1969-2019

LC_fit_2019 <- fit(LC, data = ITA, ages.fit = 0:90, years.fit = 1969:2019)
LC_boot_2019_b <- bootstrap(LC_fit_2019, nBoot = 1000, type = "semiparametric")
LC_sim_boot_2019_b <- simulate(LC_boot_2019_b, h = 25)

## ---- estrazione coorte ----

coorte <- 1954 # coorte da estrarre

# anni previsti
anni_forecast <- 2020:(2020+25-1)  # perché h = 25
eta <- 0:90

# seleziono solo combinazioni (età, anno) coerenti con la coorte scelta
indici <- which(outer(anni_forecast, eta, "-") == coorte, arr.ind = TRUE)

# estraggo i tassi simulati per la coorte scelta
# indici[,1] = posizione anno, indici[,2] = posizione età
rates_coorte <- sapply(1:nrow(indici), function(i) {
  LC_sim_boot_2019$rates[indici[i, 2], indici[i, 1], ]  # matrice (simulazioni x anni)
})

# matrice stessa dim con le probabilità di sopravvivenza t_p_x
survival_matrix <- function(rates_coorte) {
  n_sims  <- nrow(rates_coorte)
  n_years <- ncol(rates_coorte)
  
  surv <- matrix(NA, nrow = n_sims, ncol = n_years) 
  
  for (sim in 1:n_sims) {
    mu <- rates_coorte[sim, ]   # una simulazione
    cumhaz <- cumsum(mu)        # somma cumulata
    surv[sim, ] <- exp(-cumhaz) # survival probabilities
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

annuities_2019 <- array(NA, dim = 5000)
for (j in 1:5000){
  annuities_2019[j] <- annuity(v, R, surv_probs[j, ]) 
}

# istogramma
summary_2019 <-summary(annuities_2019)
hist(annuities_2019, col = "lightblue", main = " ", xlab = "scenario", ylab = " ")
points(mean(annuities_2019), 0, col = "blue", pch = 19, cex = 1.5)

## ---- coefficiente di variazione r ----

# calcolo delle componenti

Tlen <- ncol(surv_probs)       # orizzonte proiezione
v_t  <- v^(1:Tlen)             # v^t, t=1,...,T

E_Y_S <- annuities_2019_b        # E(Y|S)

# varianza condizionata Var(Y|S)
var_cond <- function(p) {
  A    <- outer(v_t, v_t)                   # v^(t+s)
  idx  <- outer(seq_along(p), seq_along(p), pmax)
  Pmax <- matrix(p[idx], nrow = length(p)) # p_{max(t,s)}
  B    <- outer(p, p)                      # p_t * p_s
  R^2 * sum(A * (Pmax - B))
}

# calcolo per ogni scenario
Var_Y_S <- apply(surv_probs, 1, var_cond)    # Var(Y|S)

# medie e varianze "esterne"
E_Y    <- mean(E_Y_S)          # E(Y)
E_Var  <- mean(Var_Y_S)        # E_rho[Var(Y|S)]
Var_E  <- var(E_Y_S)           # Var_rho[E(Y|S)]

# funzione per r(N)
r_of_N <- function(N) {
  sqrt( (1/N) * (E_Var + Var_E ) / (E_Y^2) )
}

# calcolo per diversi N
N <- c(1, 10, 100, 1000, 10000)
r_vals <- sapply(N, r_of_N)

# tabella con le componenti
results <- data.frame(
  N = N,
  r_total = r_vals,
  r_diversificabile = (1 / N) *(E_Var / E_Y^2),
  r_sistematico = Var_E / E_Y^2
)


## ---- con 1000 bootstrap ----

coorte <- 1954 # coorte da estrarre

# anni previsti
anni_forecast <- 2020:(2020+25-1)  # perché h = 25
eta <- 0:90

# seleziono solo combinazioni (età, anno) coerenti con la coorte scelta
indici <- which(outer(anni_forecast, eta, "-") == coorte, arr.ind = TRUE)

# estraggo i tassi simulati per la coorte scelta
# indici[,1] = posizione anno, indici[,2] = posizione età
rates_coorte <- sapply(1:nrow(indici), function(i) {
  LC_sim_boot_2019_b$rates[indici[i, 2], indici[i, 1], ]  # matrice (simulazioni x anni)
})

# matrice stessa dim con le probabilità di sopravvivenza t_p_x
survival_matrix <- function(rates_coorte) {
  n_sims  <- nrow(rates_coorte)
  n_years <- ncol(rates_coorte)
  
  surv <- matrix(NA, nrow = n_sims, ncol = n_years) 
  
  for (sim in 1:n_sims) {
    mu <- rates_coorte[sim, ]   # una simulazione
    cumhaz <- cumsum(mu)        # somma cumulata
    surv[sim, ] <- exp(-cumhaz) # survival probabilities
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

annuities_2019_b <- array(NA, dim = 1000)
for (j in 1:1000){
  annuities_2019_b[j] <- annuity(v, R, surv_probs[j, ]) 
}

summary_2019_b <-summary(annuities_2019_b)
hist(annuities_2019_b, freq = F)
