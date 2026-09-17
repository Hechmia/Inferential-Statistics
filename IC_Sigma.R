############################################################
# EXERCICE : Variabilité des temps de réponse
# Systèmes de détection d'intrusions
############################################################

# ==========================================================
# 1 et 2. Intervalle de confiance de sigma^2
# ==========================================================

n <- 16
S2 <- 25
alpha <- 0.05

# Degrés de liberté
ddl <- n - 1

# Quantiles du Chi-deux
q_chi_inf <- qchisq(alpha/2, df = ddl)
q_chi_sup <- qchisq(1 - alpha/2, df = ddl)

# Intervalle de confiance pour sigma^2
IC_sigma2 <- c(
  (ddl * S2) / q_chi_sup,
  (ddl * S2) / q_chi_inf
)

cat("IC à 95% pour sigma^2 :\n")
print(IC_sigma2)


# ==========================================================
# 3. Intervalle de confiance de sigma
# ==========================================================

IC_sigma <- sqrt(IC_sigma2)

cat("IC à 95% pour sigma :\n")
print(IC_sigma)


# ==========================================================
# 5. Comparaison des deux variances
# ==========================================================

n1 <- 16
S1_2 <- 25

n2 <- 13
S2_2 <- 15

df1 <- n1 - 1
df2 <- n2 - 1

# Rapport des variances empiriques
rapport <- S1_2 / S2_2

# Quantiles de Fisher
F_inf <- qf(alpha/2, df1 = df1, df2 = df2)
F_sup <- qf(1 - alpha/2, df1 = df1, df2 = df2)

# Intervalle de confiance du rapport
IC_rapport <- c(
  rapport / F_sup,
  rapport / F_inf
)

cat("Rapport S1^2/S2^2 :\n")
print(rapport)

cat("IC à 95% du rapport sigma1^2/sigma2^2 :\n")
print(IC_rapport)


# ==========================================================
# Conclusion automatique
# ==========================================================

if (IC_rapport[1] <= 1 && IC_rapport[2] >= 1) {
  cat("\nConclusion : 1 appartient à l'intervalle.\n")
  cat("On ne rejette pas l'égalité des variances au seuil de 5%.\n")
} else {
  cat("\nConclusion : 1 n'appartient pas à l'intervalle.\n")
  cat("Les variances sont significativement différentes au seuil de 5%.\n")
}

