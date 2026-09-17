############################################################
# EXERCICE : TAUX DE FRAUDE DANS UNE FINTECH
############################################################

# ==========================================================
# DONNEES
# ==========================================================

n <- 1000                 # taille de l'échantillon
x <- 120                  # nombre de transactions frauduleuses

# Estimation de la proportion
p_hat <- x / n

cat("====================================================\n")
cat("        ANALYSE DU TAUX DE FRAUDE\n")
cat("====================================================\n\n")


# ==========================================================
# 1. VERIFICATION DES CONDITIONS
# ==========================================================

np <- n * p_hat
nq <- n * (1 - p_hat)

cat("1. VERIFICATION DES CONDITIONS\n")
cat("------------------------------\n")
cat("n =", n, "\n")
cat("Nombre de transactions frauduleuses =", x, "\n")
cat("p_hat =", p_hat, "\n")
cat("n * p_hat =", np, "\n")
cat("n * (1-p_hat) =", nq, "\n\n")

if (np >= 5 && nq >= 5) {
  cat("Les conditions de l'approximation normale sont",
      "respectees.\n\n")
} else {
  cat("Les conditions de l'approximation normale ne sont",
      "pas respectees.\n\n")
}


# ==========================================================
# 2. INTERVALLE DE CONFIANCE A 95 %
# ==========================================================

alpha <- 0.05

# Quantile de la loi normale standard
z <- qnorm(1 - alpha/2)

# Erreur standard
SE <- sqrt(p_hat * (1 - p_hat) / n)

# Marge d'erreur
marge <- z * SE

# Intervalle de confiance
IC <- c(
  p_hat - marge,
  p_hat + marge
)

cat("2. INTERVALLE DE CONFIANCE A 95 %\n")
cat("------------------------------\n")
cat("Proportion observee =", p_hat, "\n")
cat("Erreur standard =", round(SE, 6), "\n")
cat("Quantile z =", round(z, 4), "\n")
cat("Marge d'erreur =", round(marge, 6), "\n")

cat("IC a 95 % = [",
    round(IC[1], 6), ";",
    round(IC[2], 6), "]\n")

cat("IC a 95 % en pourcentage = [",
    round(100 * IC[1], 2), "% ;",
    round(100 * IC[2], 2), "%]\n\n")


# ==========================================================
# 3. INTERPRETATION METIER
# ==========================================================

cat("3. INTERPRETATION METIER\n")
cat("------------------------------\n")
cat("Le taux de fraude observe est de",
    round(100 * p_hat, 2), "%.\n")

cat("L'intervalle de confiance a 95 % indique que le taux\n")
cat("reel de fraude est estime dans la plage [",
    round(100 * IC[1], 2), "% ;",
    round(100 * IC[2], 2), "%].\n\n")


# ==========================================================
# 4. TAILLE MINIMALE DE L'ECHANTILLON
# ==========================================================

# Largeur maximale demandee
largeur_max <- 0.02

# La marge d'erreur est la moitie de la largeur
epsilon <- largeur_max / 2

# Formule :
# n >= z^2 * p_hat * (1-p_hat) / epsilon^2

n_min <- ceiling(
  z^2 * p_hat * (1 - p_hat) / epsilon^2
)

cat("4. TAILLE MINIMALE DE L'ECHANTILLON\n")
cat("------------------------------\n")
cat("Largeur maximale =", largeur_max, "\n")
cat("Marge d'erreur epsilon =", epsilon, "\n")

cat("Taille minimale n =", n_min, "\n\n")


# ==========================================================
# VERIFICATION
# ==========================================================

largeur_obtenue <- 2 * z *
  sqrt(p_hat * (1 - p_hat) / n_min)

cat("VERIFICATION\n")
cat("------------------------------\n")
cat("Largeur obtenue avec n =", n_min, ":",
    round(largeur_obtenue, 6), "\n")

if (largeur_obtenue <= largeur_max) {
  cat("La condition de largeur maximale est respectee.\n")
} else {
  cat("La condition de largeur maximale n'est pas respectee.\n")
}


# ==========================================================
# RESUME
# ==========================================================

cat("\n====================================================\n")
cat("                 RESUME\n")
cat("====================================================\n")
cat("Taux de fraude observe :", round(100 * p_hat, 2), "%\n")
cat("IC 95 % : [",
    round(100 * IC[1], 2), "% ;",
    round(100 * IC[2], 2), "%]\n")
cat("Largeur souhaitee :", largeur_max, "\n")
cat("Taille minimale :", n_min, "transactions\n")
cat("====================================================\n")

