# LEE-CARTER MODEL

## ---- pacchetti ----

install.packages("demography") 
install.packages("StMoMo")
install.packages("fanplot")
install.packages("lifecontingencies")

library(demography)
library(StMoMo)
library(fanplot)

## ---- costruzione modello ----

constLC <- function(ax, bx, kt, b0x, gc, wxt, ages){
  c1 <- mean(kt[1, ], na.rm = T)
  c2 <- sum(bx[ ,1], na.rm  = T)
  list(ax = ax + c1 * bx, bx = bx / c2, kt = c2 * (kt - c1))
}

LC <- StMoMo(link = "log", staticAgeFun = T, periodAgeFun = "NP", 
             constFun = constLC)

## ---- scelta dati ----

ITA_data <- hmd.mx(country = "ITA", username = "milla.campajola@gmail.com", 
                   password = "wAczex-xacxuq-havzo4")
ITA <- StMoMoData(ITA_data, series = "total")

# fit del modello
LC_fit <- fit(LC, data = ITA, ages.fit = 0:90, years.fit = 1969:2019)

# controllo limitazioni modello
sum(LC_fit$bx) # 1
sum(LC_fit$kt) # quasi 0

ax <- LC_fit$ax
bx <- LC_fit$bx
kt <- LC_fit$kt
ages <- LC_fit$ages
years <- LC_fit$years

# plot ax
plot(ages, ax, type = "l", col = "red", xlab = "age", ylab = "",
     main = expression(alpha[x] ~ "vs" ~ x))
# plot bx
plot(ages, bx, type = "l", col = "blue", xlab = "age", ylab = "",
     main = expression(beta[x]^{(1)} ~ "vs" ~ x))
# plot kt
plot(years, kt, type = "l", col = "darkgreen", xlab = "year", ylab = "",
     main = expression(k[t]^{(1)} ~ "vs" ~ t))

# residui
LC_res <- residuals(LC_fit)
plot(LC_res, type = "colourmap", reslim = c(-3.5, 3.5))
plot(LC_res, type = "scatter", reslim = c(-3.5, 3.5))

# previsione
LC_for <- forecast(LC_fit, h = 50) # 50 anni
plot(LC_for, only.kt = TRUE)

# simulazione
set.seed(1234)
LC_sim <- simulate(LC_fit, nsim = 500, h = 50)
plot(LC_fit$years, LC_fit$kt[1, ], type = "l", xlim = c(1969, 2069), 
     ylim = c(-200, 50), xlab = "year", ylab = expression(k[t]), 
     main = "Period index")
matlines(LC_sim$kt.s$years, LC_sim$kt.s$sim[1, ,1:30], type = "l", lty = 1)

# fan chart
probs = c(2.5, 10, 25, 50, 75, 90, 97.5)
mxt <- LC_fit$Dxt / LC_fit$Ext 
matplot(LC_fit$years, t(mxt[c("65", "75", "85"), ]), xlim = c(1969, 2069), 
        ylim = c(0.0015, 0.4), pch = 20, col = "black", log = "y", 
        xlab = "year", ylab = "mortality rate") 
fan(t(LC_sim$rates["65", , ]), start = 2020, probs = probs, n.fan = 4,
    fan.col = colorRampPalette(c("darkgreen", "white")), ln = NULL)
fan(t(LC_sim$rates["75", , ]), start = 2020, probs = probs, n.fan = 4,
    fan.col = colorRampPalette(c("red", "white")), ln = NULL)
fan(t(LC_sim$rates["85", , ]), start = 2020, probs = probs, n.fan = 4,
    fan.col = colorRampPalette(c("blue", "white")), ln = NULL)
text(1973, mxt[c("65", "75", "85"), "1998"],
     labels = c("x = 65", "x = 75", "x = 85"))

# estrarre coorte 1954
plot(15:65, extractCohort(fitted(LC_fit, type = "rates"), cohort = 1954),
     type = "l", log = "y", xlab = "age", ylab = "mortality rate", 
     main = "Mortality rates for the 1954 cohort",
     xlim = c(15,90), ylim = c(0.0007, 0.12))
lines(66:90, extractCohort(LC_for$rates, cohort = 1954), lty = 2)

## ---- bootstrap ----

# periodo 1969-2019
LC_fit_2019 <- fit(LC, data = ITA, ages.fit = 0:90, years.fit = 1969:2019)
LC_boot_2019 <- bootstrap(LC_fit_2019, nBoot = 5000, type = "semiparametric")
plot(LC_boot_2019, colour = "black", nCol = 3)
LC_sim_boot_2019 <- simulate(LC_boot_2019, h = 25)
LC_for_2019 <- forecast(LC_fit_2019, h = 25)
LC_sim_2019 <- simulate(LC_fit_2019, nsim = 5000, h = 25)

mxt_2019 <- LC_fit_2019$Dxt / LC_fit_2019$Ext
mxt_hat_2019 <- fitted(LC_fit_2019, type = "rates")
mxt_central_2019 <- LC_for_2019$rates
mxt_Pred2.5_2019 <- apply(LC_sim_2019$rates, c(1, 2), quantile, probs = 0.025)
mxt_Pred97.5_2019 <- apply(LC_sim_2019$rates, c(1, 2), quantile, probs = 0.975)
mxt_hat_PU2.5_2019 <- apply(LC_sim_boot_2019$fitted, c(1, 2), quantile, probs = 0.025)
mxt_hat_PU97.5_2019 <- apply(LC_sim_boot_2019$fitted, c(1, 2), quantile, probs = 0.975)
mxt_PredPU2.5_2019 <- apply(LC_sim_boot_2019$rates, c(1, 2), quantile, probs = 0.025)
mxt_PredPU97.5_2019 <- apply(LC_sim_boot_2019$rates, c(1, 2), quantile, probs = 0.975)

x <- c("40", "60", "80")
matplot(LC_fit_2019$years, t(mxt_2019[x, ]), xlim = range(LC_fit_2019$years, LC_for_2019$years), 
        ylim = range(mxt_hat_PU97.5_2019[x, ], mxt_PredPU2.5_2019[x, ], mxt_2019[x, ]), 
        type = "p", xlab = "years",
        ylab = "mortality rates", log = "y", pch = 20, col = "black")
matlines(LC_fit_2019$years, t(mxt_hat_2019[x, ]), lty = 1, col = "black")
matlines(LC_fit_2019$years, t(mxt_hat_PU2.5_2019[x, ]), lty = 5, col = "red")
matlines(LC_fit_2019$years, t(mxt_hat_PU97.5_2019[x, ]), lty = 5, col = "red")
matlines(LC_for_2019$years, t(mxt_central_2019[x, ]), lty = 4, col = "black")
matlines(LC_sim_2019$years, t(mxt_Pred2.5_2019[x, ]), lty = 3, col = "black")
matlines(LC_sim_2019$years, t(mxt_Pred97.5_2019[x, ]), lty = 3, col = "black")
matlines(LC_sim_boot_2019$years, t(mxt_PredPU2.5_2019[x, ]), lty = 5, col = "red")
matlines(LC_sim_boot_2019$years, t(mxt_PredPU97.5_2019[x, ]), lty = 5, col = "red")
text(1970, mxt_hat_PU2.5_2019[x, "1990"], labels = c("x=40", "x=60", "x=80"))

