##########################################################################
# Ce TP étant la suite du précédent il faut que vous rechargiez votre ####
# script du TP précédent pour avoir toutes les fonctions déjà         ####
# implémentées.                                                       ####
##########################################################################

##########################################################################
## Installation de package et chargement du package, si nécessaire  ######
##########################################################################
if (!require("matlib")) {
  install.packages("matlib")
}
library(matlib)
if (!require("MASS")) {
  install.packages("MASS")  
}
library(MASS)

##########################################################################
#  Création d'une fonction d'arrondi pour les calculs  en fin de TP ######
##########################################################################
mettreValeurMatriceAZero <- function(matriceA, precision = 1e-10) {
  matriceA[abs(matriceA) < precision] <- 0
  return(matriceA)
}

##########################################################################
############################### Exercice 1  ##############################
##########################################################################


#' Echanger deux lignes d'une matrice 
#'
#' @param matriceA 
#' @param ligne1 
#' @param ligne2 
#'
#' @return matriceA avec les lignes, ligne1 et ligne2, échangées
#' @export 
#'
#' @examples
echangeDeuxLignesDUneMatrice <- function(matriceA, ligne1, ligne2) {
  # Vérification que les indices sont valides
  if (ligne1 > nrow(matriceA) || ligne2 > nrow(matriceA)) {
    stop("Les indices des lignes sont hors des limites de la matrice.")
  }
  
  # Échanger les lignes
  matriceA[c(ligne1, ligne2), ] <- matriceA[c(ligne2, ligne1), ]
  return(matriceA)
}

matriceA <- creationMatrice(c(6,-1,1,0,-3,1,1,-2,1),nbLignes = 3, nbColonnes = 3)
matriceA
matriceB <- echangeDeuxLignesDUneMatrice(matriceA,1, 3)
matriceB
all.equal(matriceB, as.matrix(rowswap(matriceA,1,3)))

#' Mutliplication d'un ligne d'une matrice par un scalaire 
#'
#' @param matriceA 
#' @param ligne 
#' @param scalaire 
#'
#' @return matriceA avec la ligne ligne qui est multiplié par le scalaire scalaire. 
#' @export
#'
#' @examples
multiplierUneLigneDUneMatriceParUnScalaire <- function(matriceA, ligne, scalaire) {
  # Vérification que l'indice est valide
  if (ligne > nrow(matriceA)) {
    stop("L'indice de la ligne est hors des limites de la matrice.")
  }
  
  # Multiplier la ligne par le scalaire
  matriceA[ligne, ] <- matriceA[ligne, ] * scalaire
  return(matriceA)
}

matriceB <- multiplierUneLigneDUneMatriceParUnScalaire(matriceA, 1,-3)
matriceB
all.equal(matriceB, as.matrix(rowmult(matriceA,1,-3)))


#' Ajout d'une ligne à une autre avec possibilité d'un coefficient multiplicateur
#'
#' @param matriceA 
#' @param ligneAAjouter 
#' @param ligneConcerne 
#' @param scalaire 
#'
#' @return la matriceA avec la ligneConcerne qui est égale à ligneConcerne +ligneAAjouter*scalaire
#' @export
#'
#' @examples
ajoutDUneLigneAUneAutre <- function(matriceA, ligneAAjouter, ligneConcerne, scalaire = 1) {
  # Vérifications des indices
  if (ligneAAjouter > nrow(matriceA) || ligneConcerne > nrow(matriceA)) {
    stop("Les indices des lignes sont hors des limites de la matrice.")
  }
  
  # Ajouter la ligneAAjouter multipliée par scalaire à ligneConcerne
  matriceA[ligneConcerne, ] <- matriceA[ligneConcerne, ] + matriceA[ligneAAjouter, ] * scalaire
  return(matriceA)
}
matriceB <- ajoutDUneLigneAUneAutre(matriceA, 3, 1, -6)
matriceB
all.equal(matriceB, as.matrix(rowadd(matriceA,3,1,-6)))

matriceB <- ajoutDUneLigneAUneAutre(matriceA, 1, 3, -1/6)
#as.matrix(fractions(matriceB))
fractions(matriceB)
all.equal(matriceB, as.matrix(rowadd(matriceA,1,3,-1/6)))

##########################################################################
############################### Exercice 2  ##############################
##########################################################################

############################### Matrice A  ###############################



############################### Matrice B  ###############################



############################### Matrice C  ###############################



############################### Matrice D  ###############################




############################### Matrice E  ###############################

############################### Matrice F  ###############################


##########################################################################
############################### Exercice 3  ##############################
##########################################################################


#' Echelonne une matrice
#'
#' @param matriceA 
#'
#' @return matriceA mais échelonnée
#' @export
#'
#' @examples
echelonneUneMatrice <- function(matriceA){
  # Vérifie si l'objet est bien une matrice
  if (!is.matrix(matriceA)) {
    stop("L'objet fourni n'est pas une matrice.")
  }
  
  n <- nrow(matriceA)  # Nombre de lignes
  m <- ncol(matriceA)  # Nombre de colonnes
  
  # Copie de la matrice pour éviter de modifier l'original
  matriceResultat <- matriceA
  
  pivotRow <- 1  # Index de la ligne où se trouve le pivot
  
  for (col in 1:m) {
    # Trouver le pivot dans la colonne courante
    maxIndex <- which.max(abs(matriceResultat[pivotRow:n, col])) + pivotRow - 1
    
    # Si le pivot est nul, passer à la colonne suivante
    if (matriceResultat[maxIndex, col] == 0) {
      next
    }
    
    # Échanger la ligne contenant le pivot avec la ligne active
    if (pivotRow != maxIndex) {
      matriceResultat <- echangeDeuxLignesDUneMatrice(matriceResultat, pivotRow, maxIndex)
    }
    
    # Normaliser la ligne du pivot pour que l'élément pivot devienne 1
    matriceResultat <- multiplierUneLigneDUneMatriceParUnScalaire(matriceResultat, pivotRow, 1 / matriceResultat[pivotRow, col])
    
    # Éliminer les autres éléments de la colonne courante
    for (ligne in 1:n) {
      if (ligne != pivotRow) {
        scalaire <- -matriceResultat[ligne, col]
        matriceResultat <- ajoutDUneLigneAUneAutre(matriceResultat, pivotRow, ligne, scalaire)
      }
    }
    
    # Passer à la prochaine ligne et colonne
    pivotRow <- pivotRow + 1
    if (pivotRow > n) {
      break
    }
  }
  
  # Arrondir les valeurs proches de zéro pour éviter les erreurs numériques
  matriceResultat <- mettreValeurMatriceAZero(matriceResultat)
  
  return(matriceResultat)
  
  
 
}

matriceG <- creationMatrice(c(0,0,1,2),2,2)
matriceG
matriceGEchelonnee <- echelonneUneMatrice(matriceG)
matriceGEchelonnee
matriceH <- creationMatrice(c(0,1,1,2),2,2)
matriceH
matriceHEchelonnee <- echelonneUneMatrice(matriceH)
matriceHEchelonnee
matriceK <- creationMatrice(c(6,-1,1,0,-3,1,1,-2,1),nbLignes = 3, nbColonnes = 3)
matriceK
matriceKEchelonnee <- echelonneUneMatrice(matriceK)
fractions(matriceKEchelonnee)

############### vérification avec l'exercice précédent ###################

all.equal(matriceAEchelonnee <- echelonneUneMatrice(matriceA), as.matrix(matriceA1))
all.equal(matriceBEchelonnee <- echelonneUneMatrice(matriceB), as.matrix(matriceB2))
all.equal(matriceCEchelonnee <- echelonneUneMatrice(matriceC), as.matrix(matriceC3))
all.equal(matriceDEchelonnee <- echelonneUneMatrice(matriceD), as.matrix(matriceD4))
all.equal(matriceEEchelonnee <- echelonneUneMatrice(matriceE), as.matrix(matriceE1))
all.equal(matriceFEchelonnee <- echelonneUneMatrice(matriceF), as.matrix(matriceF3))

##########################################################################
############################### Exercice 4  ##############################
##########################################################################

# Voir les résultats à la fin de l'exercice 5 ####

##########################################################################
############################### Exercice 5  ##############################
##########################################################################

#' Echelonne et réduit une matrice (forme échelonnée réduite)
#'
#' @param matriceA Une matrice à réduire.
#'
#' @return Une matrice en forme échelonnée réduite.
#' @export
#'
#' @examples
#' matriceA <- matrix(c(6, -1, 1, 0, -3, 1, 1, -2, 1), nrow = 3, byrow = TRUE)
#' echelonneEtReduitUneMatrice(matriceA)
echelonneEtReduitUneMatrice <- function(matriceA) {
  # Validation : vérifie que l'input est une matrice
  if (!is.matrix(matriceA)) {
    stop("L'objet fourni n'est pas une matrice.")
  }
  
  n <- nrow(matriceA)  # Nombre de lignes
  m <- ncol(matriceA)  # Nombre de colonnes
  
  # Copie de la matrice pour éviter de modifier l'original
  matriceResultat <- matriceA
  
  pivotRow <- 1  # Index de la ligne actuelle pour le pivot
  
  for (col in 1:m) {
    # Trouver le pivot dans la colonne courante
    maxIndex <- which.max(abs(matriceResultat[pivotRow:n, col])) + pivotRow - 1
    
    # Si le pivot est nul, passer à la colonne suivante
    if (matriceResultat[maxIndex, col] == 0) {
      next
    }
    
    # Échanger la ligne contenant le pivot avec la ligne active
    if (pivotRow != maxIndex) {
      matriceResultat <- echangeDeuxLignesDUneMatrice(matriceResultat, pivotRow, maxIndex)
    }
    
    # Normaliser la ligne du pivot pour que l'élément pivot devienne 1
    matriceResultat <- multiplierUneLigneDUneMatriceParUnScalaire(matriceResultat, pivotRow, 1 / matriceResultat[pivotRow, col])
    
    # Éliminer les autres éléments dans la colonne courante (au-dessus et en dessous)
    for (ligne in 1:n) {
      if (ligne != pivotRow) {
        scalaire <- -matriceResultat[ligne, col]
        matriceResultat <- ajoutDUneLigneAUneAutre(matriceResultat, pivotRow, ligne, scalaire)
      }
    }
    
    # Passer à la prochaine ligne et colonne
    pivotRow <- pivotRow + 1
    if (pivotRow > n) {
      break
    }
  }
  
  # Arrondir les petites valeurs proches de zéro pour éviter les erreurs numériques
  matriceResultat <- mettreValeurMatriceAZero(matriceResultat)
  
  return(matriceResultat)
}




matriceG <- creationMatrice(c(0,0,1,2),2,2)
matriceG
matriceGReduite <- echelonneEtReduitUneMatrice(matriceG)
matriceGReduite
matriceH <- creationMatrice(c(0,1,1,2),2,2)
matriceH
matriceHReduite <- echelonneEtReduitUneMatrice(matriceH)
matriceHReduite
matriceK <- creationMatrice(c(6,-1,1,0,-3,1,1,-2,1),nbLignes = 3, nbColonnes = 3)
matriceK
matriceKReduite <- echelonneEtReduitUneMatrice(matriceK)
matriceKReduite



(matriceAReduite <- echelonneEtReduitUneMatrice(matriceA)); echelon(matriceA)
(matriceBReduite <- echelonneEtReduitUneMatrice(matriceB)); echelon(matriceB)
(matriceCReduite <- echelonneEtReduitUneMatrice(matriceC)); echelon(matriceC)
(matriceDReduite <- echelonneEtReduitUneMatrice(matriceD)); echelon(matriceD)
(matriceEReduite <- echelonneEtReduitUneMatrice(matriceE)); echelon(matriceE)
(matriceFReduite <- echelonneEtReduitUneMatrice(matriceF)); echelon(matriceF)

##########################################################################
############################### Exercice 6  ##############################
##########################################################################

############################### Systeme R  ###############################
R <- matrix(c(1,0,3,2,1,5,-3,4,2),3,3, byrow=TRUE)
bR <- c(1,2,3)
showEqn(R,bR)
############################### Systeme S  ###############################
S <- matrix(c(0,1,2,0,0,0,3,2,1,0,-2,1,5,-1,3,0),4,4,byrow=TRUE)
bS <- c(1,4,9,13)
showEqn(S,bS)
############################### Systeme T  ###############################
T <- matrix(c(1,1,1,2,5,3,3,6,1),3,3,byrow=TRUE)
bT <- c(1,2,3)
showEqn(T,bT)
############################### Systeme U  ###############################
U <- matrix(c(1,-2,5,-4,8,2,-3,6,8),3,3, byrow=TRUE)
bU <- c(11,22,36)
showEqn(U,bU)
##########################################################################
############################### Exercice 7  ##############################
##########################################################################

#' Résolution d'un système d'équations linéaires par la méthode du pivot de Gauss
#'
#' @param matriceA Matrice des coefficients du système (n x n)
#' @param matriceB Matrice des termes constants (n x 1)
#'
#' @return La matrice augmentée (matriceA|matriceB) échelonnée et réduite, ainsi que la solution du système si elle existe.
#' @export
#'
#' @examples
#' matriceA <- creationMatrice(1:9, nbColonnes = 3, nbLignes = 3)
#' matriceB <- creationMatrice(1:3, nbColonnes = 1, nbLignes = 3)
#' resolutionSystemeMethodePivotDeGauss(matriceA, matriceB)
resolutionSystemeMethodePivotDeGauss <- function(matriceA, matriceB){
  # Vérification des dimensions
  if (!is.matrix(matriceA)) {
    stop("L'objet A fourni n'est pas une matrice.")
  }
  if (!is.matrix(matriceB)) {
    stop("L'objet B fourni n'est pas une matrice.")
  }
  if (nrow(matriceA) != nrow(matriceB)) {
    stop("Le nombre de lignes de matriceA doit correspondre au nombre de lignes de matriceB.")
  }
  
  # Construction de la matrice augmentée
  matriceAugmentee <- cbind(matriceA, matriceB)
  
  # Fonction auxiliaire pour l'échelonnement réduit
  echelonneEtReduitUneMatrice <- function(matrice) {
    n <- nrow(matrice)
    p <- ncol(matrice)
    
    for (i in 1:n) {
      # Recherche du pivot maximum
      maxRow <- which.max(abs(matrice[i:n, i])) + (i - 1)
      if (matrice[maxRow, i] == 0) {
        next  # Pas de pivot possible, continuer
      }
      
      # Échange des lignes pour placer le pivot sur la diagonale
      if (maxRow != i) {
        matrice <- echangeDeuxLignesDUneMatrice(matrice, i, maxRow)
      }
      
      # Normalisation de la ligne pour rendre le pivot égal à 1
      matrice <- multiplierUneLigneDUneMatriceParUnScalaire(matrice, i, 1 / matrice[i, i])
      
      # Élimination des éléments au-dessus et au-dessous du pivot
      for (j in 1:n) {
        if (j != i && matrice[j, i] != 0) {
          matrice <- ajoutDUneLigneAUneAutre(matrice, i, j, -matrice[j, i])
        }
      }
    }
    
    return(matrice)
  }
  
  # Réduction de la matrice augmentée
  matriceReduite <- echelonneEtReduitUneMatrice(matriceAugmentee)
  
  # Extraction de la solution
  solution <- matriceReduite[, ncol(matriceReduite), drop = FALSE]
  
  return(list(
    MatriceReduite = matriceReduite,
    Solution = solution
  ))
}


matriceA <- creationMatrice(1:9, nbColonnes = 3,nbLignes = 3)
matriceB <- creationMatrice(1:3, nbColonnes = 1,nbLignes = 3)
matriceC <- creationMatrice(1:4, nbColonnes = 1,nbLignes = 4)
matriceA
matriceB
matriceC
matriceSolutionDuSystemeAB <- resolutionSystemeMethodePivotDeGauss(matriceA, matriceB )
fractions(matriceSolutionDuSystemeAB)
fractions(gaussianElimination(matriceA,matriceB))
matriceSolutionDuSystemeAC <- resolutionSystemeMethodePivotDeGauss(matriceA, matriceC )
matriceSolutionDuSystemeAC

##########################################################################
############################### Exercice 8  ##############################
##########################################################################
R <- creationMatrice(c(1,0,3,2,1,5,3,4,2), nbColonnes = 3,nbLignes = 3)
R
BR <- creationMatrice(c(1,2,3), nbColonnes = 1,nbLignes = 3)
BR
BRGausse <- resolutionSystemeMethodePivotDeGauss(R,BR)
BRGausse

S <- creationMatrice(c(0,1,2,0,0,0,3,2,1,0,-2,1,5,-1,3,0), nbColonnes=4,nbLignes = 4)
S
BS <- creationMatrice(c(1,4,9,13), nbColonnes = 1,nbLignes = 4)
BS
BSGausse <- resolutionSystemeMethodePivotDeGauss(S,BS)
BSGausse
##########################################################################
############################### Exercice 9  ##############################
##########################################################################

############################### Systeme A  ###############################
matriceA <- creationMatrice(c(2,3,1,-1,1,2,7,3,-5), nbColonnes=3, nbLignes = 3)
matriceA
RésA <- creationMatrice(c(4,3,2), nbColonnes = 1, nbLignes = 3)
RéduiteA <- resolutionSystemeMethodePivotDeGauss(matriceA , RésA)
RéduiteA

############################### Systeme B  ###############################
matriceB <- creationMatrice(c(1,2,-1,2,0,-1,1,-2,0), nbColonnes = 3, nbLignes = 3)
matriceB
RésB <- creationMatrice(c(0,0,0),nbColonnes = 1, nbLignes = 3 )
RésB
RéduiteB <- resolutionSystemeMethodePivotDeGauss(matriceB, RésB)
RéduiteB
############################### Systeme C  ###############################
matriceC <- creationMatrice(c(4,2,-1,3,-1,1,1,1,1,1,-1,1), nbColonnes = 3, nbLignes = 4)
matriceC
RésC <- creationMatrice(c(0,3,1,-2), nbColonnes = 1, nbLignes = 4)
RésC
RéduiteC<- resolutionSystemeMethodePivotDeGauss(matriceC, RésC)
RéduiteC
#pas de solution
############################### Systeme D  ###############################
matriceD <- creationMatrice(c(1,2,-2,4,1,0,1,3,-4,2,1,0,1,-2,3,1,1,4,-6,5,0,3,0,2,0), nbColonnes = 5, nbLignes=5)
matriceD
RésD <- creationMatrice(c(0,0,0,0,0), nbColonnes = 1, nbLignes = 5)
RésD
RéduiteD <- resolutionSystemeMethodePivotDeGauss(matriceD, RésD)
RéduiteD
##########################################################################
############################### Exercice 10 ##############################
##########################################################################



##########################################################################
############################### Exercice 11 ##############################
##########################################################################



##########################################################################
############################### Exercice 12 ##############################
##########################################################################

############################### Matrice A  ###############################



############################### Matrice B  ###############################



############################### Matrice C  ###############################



##########################################################################
############################### Exercice 13 ##############################
##########################################################################



############################### Question 1 a #############################



############################### Question 1 b #############################



############################### Question 2 a #############################
# Facile à faire il faut simplement développer et utiliser le fait que k^^2 = -I
############################### Question 2 b #############################
#On montre facilement par le calcul de que l'inverse de M  est (2aI-M)/(a^2+b^2) 
############################### Question 2 c #############################

