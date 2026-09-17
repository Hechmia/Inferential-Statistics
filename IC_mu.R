############################################################
# EXERCICE : ANALYSE DES TRANSACTIONS BANCAIRES
# Les montants sont exprimés en milliers de dinars
############################################################

# ==========================================================
# 0. DONNEES
# ==========================================================

x <- c(759, 750, 755, 756, 761,
       765, 770, 752, 760, 767)

n <- length(x)

cat("====================================================\n")
cat("       ANALYSE DES TRANSACTIONS BANCAIRES\n")
cat("====================================================\n\n")


# ==========================================================
# 1. ESTIMATION DE LA MOYENNE ET ERREUR STANDARD
# ==========================================================

# Moyenne empirique
x_bar <- mean(x)

# Variance empirique corrigée
S2 <- var(x)

# Ecart-type empirique corrigé
S <- sd(x)

# Erreur standard de la moyenne
SE <- S / sqrt(n)

cat("1. ESTIMATION\n")
cat("-----------------------------\n")
cat("Taille de l'échantillon n =", n, "\n")
cat("Moyenne empirique =", round(x_bar, 4), "\n")
cat("Variance corrigée S^2 =", round(S2, 4), "\n")
cat("Ecart-type S =", round(S, 4), "\n")
cat("Erreur standard =", round(SE, 4), "\n\n")


# ==========================================================
# 2. INTERVALLES DE CONFIANCE POUR LA MOYENNE
# ==========================================================

# ----------------------------------------------------------
# IC à 90 %
# ----------------------------------------------------------

alpha90 <- 0.10

# Quantile de Student
t90 <- qt(1 - alpha90/2, df = n - 1)

# Marge d'erreur
marge90 <- t90 * SE

# Intervalle de confiance
IC90 <- c(
  x_bar - marge90,
  x_bar + marge90
)

# ----------------------------------------------------------
# IC à 99 %
# ----------------------------------------------------------

alpha99 <- 0.01

# Quantile de Student
t99 <- qt(1 - alpha99/2, df = n - 1)

# Marge d'erreur
marge99 <- t99 * SE

# Intervalle de confiance
IC99 <- c(
  x_bar - marge99,
  x_bar + marge99
)


cat("2. INTERVALLES DE CONFIANCE POUR LA MOYENNE\n")
cat("-----------------------------\n")

cat("IC à 90 %\n")
cat("Quantile de Student =", round(t90, 4), "\n")
cat("Marge d'erreur =", round(marge90, 4), "\n")
cat("IC90% = [",
    round(IC90[1], 4), ";",
    round(IC90[2], 4), "]\n\n")

cat("IC à 99 %\n")
cat("Quantile de Student =", round(t99, 4), "\n")
cat("Marge d'erreur =", round(marge99, 4), "\n")
cat("IC99% = [",
    round(IC99[1], 4), ";",
    round(IC99[2], 4), "]\n\n")


# ==========================================================
# 3. NOMBRE DE JOURS NECESSAIRE
# ==========================================================

# On souhaite :
# Longueur de l'IC à 95 % <= 1
#
# L'énoncé impose :
# t_(n-1, alpha/2) ≈ 2
#
# Longueur de l'intervalle :
# L = 2 * t * S / sqrt(n)

t_approx <- 2
L_max <- 1

# Résolution :
# 2 * t * S / sqrt(n) <= L_max

n_min <- ceiling((2 * t_approx * S / L_max)^2)

# Vérification de la longueur avec n_min
L <- 2 * t_approx * S / sqrt(n_min)


cat("3. NOMBRE DE JOURS NECESSAIRE\n")
cat("-----------------------------\n")
cat("Niveau de confiance = 95 %\n")
cat("Valeur approximative de t =", t_approx, "\n")
cat("Longueur maximale souhaitée =", L_max, "\n")
cat("Nombre minimal de jours =", n_min, "\n")
cat("Longueur obtenue =", round(L, 4), "\n\n")


# ==========================================================
# 4. RESUME DES RESULTATS
# ==========================================================

cat("====================================================\n")
cat("                 RESUME DES RESULTATS\n")
cat("====================================================\n")

cat("Moyenne              :", round(x_bar, 2), "milliers DT\n")
cat("Variance S^2         :", round(S2, 2), "\n")
cat("Ecart-type S         :", round(S, 2), "milliers DT\n")
cat("Erreur standard      :", round(SE, 2), "milliers DT\n")
cat("IC 90 %              : [",
    round(IC90[1], 2), ";",
    round(IC90[2], 2), "]\n")
cat("IC 99 %              : [",
    round(IC99[1], 2), ";",
    round(IC99[2], 2), "]\n")
cat("Nombre minimal jours :", n_min, "\n")
cat("====================================================\n")

