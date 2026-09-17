# --- Données du problème ---
n1 <- 12
n2 <- 15
S1_sq <- 16
S2_sq <- 4
alpha <- 0.05

# 1. Rapport des variances observées
ratio_obs <- S1_sq / S2_sq

# 2. Quantiles de la loi de Fisher (11, 14) et (14, 11)
f_quant1 <- qf(1 - alpha/2, df1 = n1 - 1, df2 = n2 - 1) # ~ 3.359
f_quant2 <- qf(1 - alpha/2, df1 = n2 - 1, df2 = n1 - 1) # ~ 3.005

# Intervalle de confiance
ic_inf <- ratio_obs / f_quant1
ic_sup <- ratio_obs * f_quant2

cat("--- RESULTATS ---\n")
cat("Rapport S1^2 / S2^2 :", ratio_obs, "\n")
cat("Intervalle de confiance a 95% : [", round(ic_inf, 3), ";", round(ic_sup, 3), "]\n")

# 3. Calcul de la p-valeur du test F
p_valeur <- 2 * (1 - pf(ratio_obs, df1 = n1 - 1, df2 = n2 - 1))
cat("p-valeur du test :", round(p_valeur, 4), "\n")

if(p_valeur < alpha) {
  cat("Decision : Rejet de H0. Les variances sont significativement differentes.\n")
} else {
  cat("Decision : Non-rejet de H0. Aucune difference significative de variance.\n")
}
