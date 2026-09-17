############################################################
# EXERCICE : ANALYSE DES SCORES DE SATISFACTION
# Algorithme d'analyse de sentiment
############################################################

# ==========================================================
# 0. DONNEES
# ==========================================================

x <- c(72, 65, 78, 80, 74,
       77, 71, 68, 76, 75)

n <- length(x)

cat("====================================================\n")
cat("     ANALYSE DES SCORES DE SATISFACTION\n")
cat("====================================================\n\n")


# ==========================================================
# 1. ESTIMATEURS DU MAXIMUM DE VRAISEMBLANCE
# ==========================================================

# Estimateur MV de mu
mu_MV <- mean(x)

# Estimateur MV de sigma^2
# Attention : division par n
sigma2_MV <- mean((x - mu_MV)^2)

cat("1. ESTIMATEURS DU MAXIMUM DE VRAISEMBLANCE\n")
cat("--------------------------------------------\n")
cat("Estimateur MV de mu =", mu_MV, "\n")
cat("Estimateur MV de sigma^2 =", sigma2_MV, "\n\n")


# ==========================================================
# 2. MOYENNE, VARIANCE EMPIRIQUE ET ESTIMATEUR SANS BIAIS
# ==========================================================

# Moyenne empirique
x_bar <- mean(x)

# Variance empirique corrigée
S2 <- var(x)

# Ecart-type corrigé
S <- sd(x)

cat("2. MOYENNE ET VARIANCE\n")
cat("----------------------\n")
cat("Moyenne empirique =", x_bar, "\n")
cat("Variance empirique corrigee S^2 =", S2, "\n")
cat("Ecart-type S =", S, "\n\n")


# ==========================================================
# 3. INTERVALLE DE CONFIANCE POUR MU
# ==========================================================

# Statistique de Student :
# T = (Xbar - mu) / (S / sqrt(n))

alpha <- 0.05

# Quantile de Student
t_crit <- qt(1 - alpha/2, df = n - 1)

# Erreur standard
SE <- S / sqrt(n)

# Marge d'erreur
marge <- t_crit * SE

# Intervalle de confiance
IC_mu <- c(
  x_bar - marge,
  x_bar + marge
)

cat("3. INTERVALLE DE CONFIANCE POUR MU\n")
cat("-----------------------------------\n")
cat("Degres de liberte =", n - 1, "\n")
cat("Quantile de Student =", round(t_crit, 4), "\n")
cat("Erreur standard =", round(SE, 4), "\n")
cat("Marge d'erreur =", round(marge, 4), "\n")

cat("IC a 95% pour mu = [",
    round(IC_mu[1], 4), ";",
    round(IC_mu[2], 4), "]\n\n")


# ==========================================================
# 4. NOMBRE D'AVIS NECESSAIRE
# ==========================================================

# L'enonce impose :
# t_(n-1, alpha/2) ≈ 2
#
# Longueur de l'intervalle :
# L = 2 * t * S / sqrt(n)
#
# On veut L <= 1

t_approx <- 2
L_max <- 1

n_min <- ceiling(
  (2 * t_approx * S / L_max)^2
)

# Verification
L_verif <- 2 * t_approx * S / sqrt(n_min)

cat("4. NOMBRE D'AVIS NECESSAIRE\n")
cat("----------------------------\n")
cat("Longueur maximale =", L_max, "\n")
cat("Valeur approx. de t =", t_approx, "\n")
cat("Nombre minimal d'avis =", n_min, "\n")
cat("Longueur obtenue =", round(L_verif, 4), "\n\n")


# ==========================================================
# 5. INTERVALLE DE CONFIANCE POUR SIGMA^2
# ==========================================================

# Statistique pivot :
# ((n-1)*S^2) / sigma^2 ~ Chi-deux(n-1)

# Quantiles du Chi-deux
chi_inf <- qchisq(alpha/2, df = n - 1)
chi_sup <- qchisq(1 - alpha/2, df = n - 1)

# Intervalle pour sigma^2
IC_sigma2 <- c(
  (n - 1) * S2 / chi_sup,
  (n - 1) * S2 / chi_inf
)

# Intervalle pour sigma
IC_sigma <- sqrt(IC_sigma2)

cat("5. INTERVALLES DE CONFIANCE POUR LA VARIANCE\n")
cat("---------------------------------------------\n")
cat("Quantile Chi2 inferieur =",
    round(chi_inf, 4), "\n")
cat("Quantile Chi2 superieur =",
    round(chi_sup, 4), "\n")

cat("IC a 95% pour sigma^2 = [",
    round(IC_sigma2[1], 4), ";",
    round(IC_sigma2[2], 4), "]\n")

cat("IC a 95% pour sigma = [",
    round(IC_sigma[1], 4), ";",
    round(IC_sigma[2], 4), "]\n\n")


# ==========================================================
# 6. COMPARAISON AVEC LE DEUXIEME SYSTEME
# ==========================================================

n1 <- 10
S1_2 <- S2

n2 <- 13
S2_2 <- 18.42

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

cat("6. COMPARAISON DES DEUX SYSTEMES\n")
cat("---------------------------------\n")
cat("S1^2 =", S1_2, "\n")
cat("S2^2 =", S2_2, "\n")
cat("n1 =", n1, "\n")
cat("n2 =", n2, "\n")
cat("Rapport S1^2/S2^2 =",
    round(rapport, 4), "\n")

cat("Quantile F inferieur =",
    round(F_inf, 4), "\n")
cat("Quantile F superieur =",
    round(F_sup, 4), "\n")

cat("IC a 95% du rapport = [",
    round(IC_rapport[1], 4), ";",
    round(IC_rapport[2], 4), "]\n\n")


# ==========================================================
# CONCLUSION AUTOMATIQUE
# ==========================================================

if (IC_rapport[1] <= 1 && IC_rapport[2] >= 1) {
  
  cat("CONCLUSION :\n")
  cat("1 appartient a l'intervalle de confiance.\n")
  cat("On ne rejette pas l'egalite des variances au seuil de 5%.\n")
  
} else {
  
  cat("CONCLUSION :\n")
  cat("1 n'appartient pas a l'intervalle de confiance.\n")
  cat("Les variances sont significativement differentes au seuil de 5%.\n")
}


# ==========================================================
# RESUME
# ==========================================================

cat("\n====================================================\n")
cat("                 RESUME DES RESULTATS\n")
cat("====================================================\n")

cat("Moyenne =", round(x_bar, 2), "\n")
cat("Variance MV =", round(sigma2_MV, 2), "\n")
cat("Variance sans biais S^2 =", round(S2, 2), "\n")
cat("Ecart-type S =", round(S, 2), "\n")

cat("IC 95% de mu = [",
    round(IC_mu[1], 2), ";",
    round(IC_mu[2], 2), "]\n")

cat("Nombre minimal d'avis =", n_min, "\n")

cat("IC 95% de sigma^2 = [",
    round(IC_sigma2[1], 2), ";",
    round(IC_sigma2[2], 2), "]\n")

cat("IC 95% de sigma = [",
    round(IC_sigma[1], 2), ";",
    round(IC_sigma[2], 2), "]\n")

cat("IC 95% du rapport sigma1^2/sigma2^2 = [",
    round(IC_rapport[1], 2), ";",
    round(IC_rapport[2], 2), "]\n")

cat("====================================================\n")