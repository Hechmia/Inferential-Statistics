############################################################
# EXERCICE : LATENCE D'UN RESEAU MOBILE
############################################################

cat("====================================================\n")
cat("          ANALYSE DE LA LATENCE DU RESEAU\n")
cat("====================================================\n\n")


# ==========================================================
# 1. PREMIER ECHANTILLON : SIGMA CONNU
# ==========================================================

n1 <- 50
xbar1 <- 42
sigma <- 5

# Quantile normal
z <- 1.96

# Erreur standard
SE1 <- sigma / sqrt(n1)

# Marge d'erreur
marge1 <- z * SE1

# Intervalle de confiance
IC1 <- c(
  xbar1 - marge1,
  xbar1 + marge1
)

# Largeur
largeur1 <- IC1[2] - IC1[1]


cat("1. IC 95% - SIGMA CONNU\n")
cat("------------------------\n")
cat("n =", n1, "\n")
cat("Moyenne =", xbar1, "ms\n")
cat("Sigma =", sigma, "ms\n")
cat("Erreur standard =", round(SE1, 4), "\n")
cat("Marge d'erreur =", round(marge1, 4), "\n")
cat("IC 95% = [",
    round(IC1[1], 4), ";",
    round(IC1[2], 4), "] ms\n")
cat("Largeur =", round(largeur1, 4), "ms\n\n")


# ==========================================================
# 2. DEUXIEME ECHANTILLON : SIGMA INCONNU
# ==========================================================

X <- c(41, 40, 39, 45, 38,
       42, 40, 44, 39, 42)

n2 <- length(X)

# Moyenne empirique
xbar2 <- mean(X)

# Variance empirique corrigée
S2 <- var(X)

# Ecart-type empirique corrige
S <- sd(X)


cat("2. STATISTIQUES DU DEUXIEME ECHANTILLON\n")
cat("----------------------------------------\n")
cat("n =", n2, "\n")
cat("Moyenne empirique =", round(xbar2, 4), "ms\n")
cat("Variance corrigee S^2 =", round(S2, 4), "ms^2\n")
cat("Ecart-type corrige S =", round(S, 4), "ms\n\n")


# ==========================================================
# 2.3. IC 95% AVEC LA LOI DE STUDENT
# ==========================================================

t_crit <- 2.262

# Erreur standard
SE2 <- S / sqrt(n2)

# Marge d'erreur
marge2 <- t_crit * SE2

# Intervalle de confiance
IC2 <- c(
  xbar2 - marge2,
  xbar2 + marge2
)

# Largeur
largeur2 <- IC2[2] - IC2[1]


cat("2.3. IC 95% - SIGMA INCONNU\n")
cat("---------------------------\n")
cat("Degres de liberte =", n2 - 1, "\n")
cat("Quantile de Student =", t_crit, "\n")
cat("Erreur standard =", round(SE2, 4), "\n")
cat("Marge d'erreur =", round(marge2, 4), "\n")
cat("IC 95% = [",
    round(IC2[1], 4), ";",
    round(IC2[2], 4), "] ms\n")
cat("Largeur =", round(largeur2, 4), "ms\n\n")


# ==========================================================
# 3. COMPARAISON DES PRECISIONS
# ==========================================================

cat("3. COMPARAISON DES INTERVALLES\n")
cat("------------------------------\n")
cat("Largeur avec sigma connu   =", round(largeur1, 4), "ms\n")
cat("Largeur avec sigma inconnu =", round(largeur2, 4), "ms\n")

if (largeur2 > largeur1) {
  cat("L'intervalle avec sigma inconnu est plus large.\n")
}

cat("\n")


# ==========================================================
# 5. INTERVALLE DE CONFIANCE POUR SIGMA^2
# ==========================================================

# Statistique pivot :
# ((n-1)*S^2) / sigma^2 ~ Chi-deux(n-1)

df <- n2 - 1

# Quantiles fournis dans l'enonce
chi2_975 <- 19.023
chi2_025 <- 2.70

# IC pour sigma^2
IC_sigma2 <- c(
  df * S2 / chi2_975,
  df * S2 / chi2_025
)


cat("5. IC 95% POUR LA VARIANCE\n")
cat("--------------------------\n")
cat("Degres de liberte =", df, "\n")
cat("(n-1)*S^2 =", round(df * S2, 4), "\n")

cat("IC 95% pour sigma^2 = [",
    round(IC_sigma2[1], 4), ";",
    round(IC_sigma2[2], 4), "] ms^2\n\n")


# ==========================================================
# 5.3. IC POUR SIGMA
# ==========================================================

IC_sigma <- sqrt(IC_sigma2)

cat("5.3. IC 95% POUR L'ECART-TYPE\n")
cat("-----------------------------\n")
cat("IC 95% pour sigma = [",
    round(IC_sigma[1], 4), ";",
    round(IC_sigma[2], 4), "] ms\n\n")


# ==========================================================
# 5.4. CRITERE DE STABILITE
# ==========================================================

seuil <- 3

cat("5.4. CRITERE DE STABILITE\n")
cat("-------------------------\n")
cat("Seuil de stabilite : sigma <", seuil, "ms\n")

if (IC_sigma[2] < seuil) {
  
  cat("Conclusion : le critere de stabilite est respecte\n")
  cat("avec un niveau de confiance de 95%.\n")
  
} else if (IC_sigma[1] >= seuil) {
  
  cat("Conclusion : le critere de stabilite n'est pas\n")
  cat("respecte avec un niveau de confiance de 95%.\n")
  
} else {
  
  cat("Conclusion : les donnees ne permettent pas de\n")
  cat("conclure que sigma est inferieur a 3 ms.\n")
  cat("Le seuil de 3 ms appartient a l'intervalle de confiance.\n")
}


# ==========================================================
# RESUME
# ==========================================================

cat("\n====================================================\n")
cat("                 RESUME DES RESULTATS\n")
cat("====================================================\n")

cat("IC 95% (sigma connu)   : [",
    round(IC1[1], 2), ";",
    round(IC1[2], 2), "] ms\n")

cat("IC 95% (sigma inconnu) : [",
    round(IC2[1], 2), ";",
    round(IC2[2], 2), "] ms\n")

cat("Largeur IC sigma connu   :",
    round(largeur1, 2), "ms\n")

cat("Largeur IC sigma inconnu :",
    round(largeur2, 2), "ms\n")

cat("Variance S^2             :",
    round(S2, 2), "ms^2\n")

cat("IC 95% sigma^2           : [",
    round(IC_sigma2[1], 2), ";",
    round(IC_sigma2[2], 2), "] ms^2\n")

cat("IC 95% sigma             : [",
    round(IC_sigma[1], 2), ";",
    round(IC_sigma[2], 2), "] ms\n")

cat("====================================================\n")