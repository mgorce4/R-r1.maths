##########################################################################
## Installation de package et chargement du package, si nécessaire #######
##########################################################################
if (!require("MASS")) {
  install.packages("MASS")  
}
library(MASS)
if (!require("igraph")) {
  install.packages("igraph")  
}
library(igraph)
##########################################################################
############################### Exercice 1  ##############################
##########################################################################

#' Création d'une matrice vide de dimensions données.
#'
#' @param nbLignes 
#' @param nbColonnes 
#'
#' @return une matrice vide de dimensions données. 
#' @export
#'
#' @examples
#' creationMatriceVide(nbLignes=3,nbColonnes=3)
creationMatriceVide <- function(nbLignes, nbColonnes) {
  # Vérifie que les dimensions sont positives
  if (nbLignes <= 0 || nbColonnes <= 0) {
    stop("Les dimensions doivent être positives.")
  }
  
  # Crée une matrice remplie de NA avec les dimensions spécifiées
  matrice <- matrix(NA, nrow = nbLignes, ncol = nbColonnes)
  
  return(matrice)
}

#' Création d'une matrice 
#'
#' @param valeurs 
#' @param nbLignes 
#' @param nbColonnes 
#'
#' @return Une matrice aux dimensions demandées mais sans valeurs si le vecteur 
#' valeurs n'a pas assez de valeurs. 
#' @export
#'
#' @examples
creationMatrice <- function(valeurs, nbLignes, nbColonnes) {
  # Vérifie que les dimensions sont positives
  if (nbLignes <= 0 || nbColonnes <= 0) {
    stop("Les dimensions doivent être positives.")
  }
  
  # Calcul du nombre d'éléments requis
  nbElementsRequis <- nbLignes * nbColonnes
  
  # Complète les valeurs si elles sont insuffisantes
  if (length(valeurs) < nbElementsRequis) {
    valeurs <- c(valeurs, rep(NA, nbElementsRequis - length(valeurs)))
  }
  
  # Crée la matrice avec les dimensions spécifiées
  matrice <- matrix(valeurs[1:nbElementsRequis], nrow = nbLignes, ncol = nbColonnes, byrow = TRUE)
  
  return(matrice)
}


premiereMatrice <- creationMatrice(1:8, nbLignes= 3 , nbColonnes = 3)
premiereMatrice
secondeMatrice <- creationMatrice(1:9, nbLignes= 3 , nbColonnes = 3)
secondeMatrice



##########################################################################
############################### Exercice 2  ##############################
##########################################################################

#' Additionne deux matrices
#'
#' @param matriceA 
#' @param matriceB 
#'
#' @return matriceA + matriceB
#' @export
#'
#' @examples
additionDeDeuxMatrices <- function(matriceA, matriceB) {
  # Vérifie si les dimensions des deux matrices sont identiques
  if (!all(dim(matriceA) == dim(matriceB))) {
    stop("Les dimensions des matrices ne sont pas compatibles pour l'addition.")
  }
  
  # Réalise l'addition élément par élément
  matriceResultat <- matriceA + matriceB
  
  return(matriceResultat)
}

troisiemeMatrice <-creationMatrice(rep(4:6, 3), nbColonnes = 3, nbLignes = 3)
troisiemeMatrice
quatriemeMatrice <- creationMatrice(rep(1:3,2),nbLignes = 2, nbColonnes = 3)
quatriemeMatrice
cinquiemeMatrice <- additionDeDeuxMatrices(secondeMatrice,troisiemeMatrice)
cinquiemeMatrice
all.equal(cinquiemeMatrice, secondeMatrice+troisiemeMatrice)
sixiemeMatrice <- additionDeDeuxMatrices(secondeMatrice, quatriemeMatrice)
sixiemeMatrice
#all.equal(sixiemeMatrice, secondeMatrice+quatriemeMatrice)
#secondeMatrice+quatriemeMatrice

##########################################################################
############################### Exercice 3  ##############################
##########################################################################

############################### Question 1 ############################### 
MatriceA <- creationMatrice(c(1, -0.5, 1/3, 0, 0.5, 0), nbLignes= 2 , nbColonnes = 3)
MatriceA


MatriceB <- creationMatrice(c(1, 0.5, 1, 9, 5, sqrt(2)), nbLignes= 2 , nbColonnes = 3)
MatriceB

MatriceAB <-additionDeDeuxMatrices(MatriceA, MatriceB)
MatriceAB

############################### Question 2 ############################### 
MatriceC <- creationMatrice(c(0,-1,0,6,0,0,0,0,2), nbLignes= 3 , nbColonnes = 3)
MatriceC


MatriceD <- creationMatrice(c(1 ,-1 ,0 ,6 ,1 ,1 ,1 ,1, 2), nbLignes=3  , nbColonnes = 3)
MatriceD

MatriceCD <-additionDeDeuxMatrices(MatriceC, MatriceD)
MatriceCD

##########################################################################
############################### Exercice 4  ##############################
##########################################################################

#' Permet de faire la conversion d'une matrice quelconque en matrice boolénne
#' en prenant le postulat que si une valeur est différente de 0, c'est qu'en booléen 
#' cette valeur vaut 1. 
#'
#' @param uneMatrice 
#'
#' @return une matrice booleenne 
#' @export
#'
#' @examples
#' matriceBoolenneA <- convertionMatriceEnMatriceBoolenne(matriceA)
#' 
convertionMatriceEnMatriceBoolenne <- function(uneMatrice) {
  # Vérifie si uneMatrice est bien une matrice
  if (!is.matrix(uneMatrice)) {
    stop("L'objet fourni n'est pas une matrice.")
  }
  
  # Applique la conversion : chaque valeur différente de 0 devient 1, sinon 0
  matriceBoolenne <- ifelse(uneMatrice != 0, 1, 0)
  
  return(matriceBoolenne)
}


additionDeDeuxMatricesBoolennes <- function(matriceA, matriceB) {
  # Vérifie si les dimensions des deux matrices sont identiques
  if (!all(dim(matriceA) == dim(matriceB))) {
    stop("Les dimensions des matrices ne sont pas compatibles pour l'addition.")
  }
  
  # Effectue l'addition élément par élément (valeurs limitées à 1 pour booléen)
  matriceResultat <- pmin(matriceA + matriceB, 1)
  
  return(matriceResultat)
}

matriceBoolenneA <- convertionMatriceEnMatriceBoolenne(MatriceA)
matriceBoolenneB <- convertionMatriceEnMatriceBoolenne(MatriceC[2:3,])
matriceBoolenneA
matriceBoolenneB
matriceBoolenneAPlusB <- additionDeDeuxMatricesBoolennes(matriceBoolenneA,matriceBoolenneB)
matriceBoolenneAPlusB

##########################################################################
############################### Exercice 5  ##############################
##########################################################################
transposeDUneMatrice <- function(uneMatrice) {
  # Vérifie si uneMatrice est bien une matrice
  if (!is.matrix(uneMatrice)) {
    stop("L'objet fourni n'est pas une matrice.")
  }
  
  # Obtenir le nombre de lignes et de colonnes
  nbLignes <- nrow(uneMatrice)
  nbColonnes <- ncol(uneMatrice)
  
  # Crée une matrice vide avec les dimensions inversées
  matriceTransposee <- matrix(NA, nrow = nbColonnes, ncol = nbLignes)
  
  # Remplit la matrice transposée manuellement
  for (i in 1:nbLignes) {
    for (j in 1:nbColonnes) {
      matriceTransposee[j, i] <- uneMatrice[i, j]
    }
  }
  
  return(matriceTransposee)
}


transposeeMatriceA <- transposeDUneMatrice(MatriceA)
transposeeMatriceA
all.equal(transposeeMatriceA, t(MatriceA))

##########################################################################
############################### Exercice 6  ##############################
##########################################################################
MatriceE <- creationMatrice(c(1,-1,2,5,-1,0.5,2,0.4,0,3,2,0.1), nbLignes= 3 , nbColonnes = 4)
MatriceE


MatriceF <- creationMatrice(c(1,-1,2,2,0,32,3,-9,0), nbLignes=3  , nbColonnes = 3)
MatriceF

transposeeMatriceE <- transposeDUneMatrice(MatriceE)
transposeeMatriceE

transposeeMatriceF <- transposeDUneMatrice(MatriceF)
transposeeMatriceF
##########################################################################
############################### Exercice 7  ##############################
##########################################################################

#' Produit de deux matrices
#'
#' @param matriceA 
#' @param matriceB 
#'
#' @return matriceA * matriceB
#' @export
#'
#' @examples
produitDeDeuxMatrices <- function(matriceA, matriceB) {
  # Vérifie si les deux objets sont des matrices
  if (!is.matrix(matriceA) || !is.matrix(matriceB)) {
    stop("Les deux objets fournis doivent être des matrices.")
  }
  
  # Vérifie si les dimensions sont compatibles pour la multiplication
  if (ncol(matriceA) != nrow(matriceB)) {
    stop("Les dimensions des matrices ne sont pas compatibles pour la multiplication.")
  }
  
  # Initialisation de la matrice résultat
  matriceResultat <- matrix(0, nrow = nrow(matriceA), ncol = ncol(matriceB))
  
  # Calcul manuel du produit matriciel
  for (i in 1:nrow(matriceA)) {
    for (j in 1:ncol(matriceB)) {
      matriceResultat[i, j] <- sum(matriceA[i, ] * matriceB[, j])
    }
  }
  
  return(matriceResultat)
}

matriceA <- creationMatrice(c(1,-0.5,fractions(1/3),0,0.5,0), 2,3)
matriceB <- creationMatrice(c(1,0.5,1,9,5,sqrt(2)),2,3)
matriceAB <- produitDeDeuxMatrices(matriceA, matriceB)
matriceAB
matriceC <- creationMatrice(c(0,-1,0,6,0,0,0,0,2), 3,3)
matriceD <- creationMatrice(c(1,-1,0,6,1,1,1,1,2), 3,3)
matriceCD <- produitDeDeuxMatrices(matriceC, matriceD)
matriceCD
all.equal(matriceCD, matriceC %*% matriceD)


##########################################################################
############################### Exercice 8  ##############################
##########################################################################

############################### Question 1 ############################### 
MatriceG <- creationMatrice(c(1,2,0,3,-1,0,0,4), nbLignes=4  , nbColonnes = 2)
MatriceG

MatriceH <- creationMatrice(c(0,-1,0,1,1,1), nbLignes=2  , nbColonnes = 3)
MatriceH
matriceGH <- produitDeDeuxMatrices(MatriceG, MatriceH)
matriceGH
matriceHG <- produitDeDeuxMatrices(MatriceH, MatriceG)
matriceHG


############################### Question 2 ############################### 
MatriceI <- creationMatrice(c(1,2,3,0,0,-1), nbLignes=2  , nbColonnes = 3)
MatriceI
MatriceJ <- creationMatrice(c(1,0,1,0,1,2), nbLignes=3  , nbColonnes = 2)
MatriceJ

matriceIJ <- produitDeDeuxMatrices(MatriceI, MatriceJ)
matriceIJ

MatriceJI <- produitDeDeuxMatrices(MatriceJ, MatriceI)
MatriceJI

##########################################################################
############################### Exercice 9  ##############################
##########################################################################
produitDeDeuxMatricesBooleennes <- function(matriceBoolenneA, matriceBoolenneB) {
  # Vérifie si les deux objets sont des matrices booléennes
  if (!is.matrix(matriceBoolenneA) || !is.matrix(matriceBoolenneB)) {
    stop("Les deux objets fournis doivent être des matrices booléennes.")
  }
  
  # Vérifie si les dimensions sont compatibles pour la multiplication
  if (ncol(matriceBoolenneA) != nrow(matriceBoolenneB)) {
    stop("Les dimensions des matrices ne sont pas compatibles pour la multiplication.")
  }
  
  # Initialisation de la matrice résultat booléenne
  matriceResultat <- matrix(0, nrow = nrow(matriceBoolenneA), ncol = ncol(matriceBoolenneB))
  
  # Effectuer le produit matriciel booléen
  for (i in 1:nrow(matriceBoolenneA)) {
    for (j in 1:ncol(matriceBoolenneB)) {
      # Calcul du produit booléen : faire un "OU" sur le produit de chaque élément
      matriceResultat[i, j] <- as.integer(any(matriceBoolenneA[i, ] & matriceBoolenneB[, j]))
    }
  }
  
  return(matriceResultat)
}

transposeDUneMatriceV2 <- function(uneMatrice) {
  # Vérifie si l'objet est bien une matrice
  if (!is.matrix(uneMatrice)) {
    stop("L'objet fourni n'est pas une matrice.")
  }
  
  # Obtenir le nombre de lignes et de colonnes de la matrice originale
  nbLignes <- nrow(uneMatrice)
  nbColonnes <- ncol(uneMatrice)
  
  # Crée une matrice vide avec les dimensions inversées
  matriceTransposee <- matrix(NA, nrow = nbColonnes, ncol = nbLignes)
  
  # Remplir la matrice transposée manuellement
  for (i in 1:nbLignes) {
    for (j in 1:nbColonnes) {
      matriceTransposee[j, i] <- uneMatrice[i, j]
    }
  }
  
  return(matriceTransposee)
}

matriceBoolenneA <- convertionMatriceEnMatriceBoolenne(matriceA)
matriceBoolenneB <- transposeDUneMatriceV2(convertionMatriceEnMatriceBoolenne(matriceC[2:3,]))
matriceBoolenneA
matriceBoolenneB
matriceBoolenneAB <- produitDeDeuxMatricesBooleennes(matriceBoolenneA,matriceBoolenneB)
matriceBoolenneAB

##########################################################################
############################### Exercice 10 ##############################
##########################################################################


############################### Question 1 ############################### 

############################ Questions 2 et 3 ############################ 

############################## Réflexivité ###############################


############################## Symetrie ##################################

############################## Antisymetrie ##############################


############################## Transitivité ##############################


##########################################################################
############################### Exercice 11 ##############################
##########################################################################

matriceR <- creationMatrice(c(1,1,0,1,0,1,1,1,0,0,0,1,1,1,1,1,0,1,1,1,0,0,1,1,1), nbLignes = 5, nbColonnes = 5)
matriceR
matriceS <- creationMatrice(c(1,1,0,0,1,1,0,0,1,1,1,0,0,0,0,1), nbLignes = 4, nbColonnes = 4)
matriceS
matriceT <- creationMatrice(c(1,1,0,0,1,0,1,0,0,0,0,1,1,1,0,0,1,0,1,0,0,0,0,0,1), nbLignes = 5, nbColonnes = 5)
matriceT

matriceReflexive <- function(MatriceM) {
  # Vérifie si l'objet est une matrice
  if (!is.matrix(MatriceM)) {
    stop("L'objet fourni n'est pas une matrice.")
  }
  
  # Vérifie si la matrice est carrée (obligatoire pour la réflexivité)
  if (nrow(MatriceM) != ncol(MatriceM)) {
    stop("La matrice doit être carrée pour vérifier la réflexivité.")
  }
  
  # Vérifie si tous les éléments de la diagonale principale sont égaux à 1
  estReflexive <- all(diag(MatriceM) == 1)
  
  # Retourne le résultat
  if (estReflexive) {
    cat("La matrice est réflexive.\n")
  } else {
    cat("La matrice n'est pas réflexive.\n")
  }
  
  return(estReflexive)
}

matriceSymétrique <- function(MatriceM) {
  # Vérifie si l'objet est une matrice
  if (!is.matrix(MatriceM)) {
    stop("L'objet fourni n'est pas une matrice.")
  }
  
  # Vérifie si la matrice est carrée (obligatoire pour tester la symétrie)
  if (nrow(MatriceM) != ncol(MatriceM)) {
    stop("La matrice doit être carrée pour vérifier la symétrie.")
  }
  
  # Vérifie si la matrice est égale à sa transposée
  estSymétrique <- all(MatriceM == t(MatriceM))
  
  # Retourne le résultat
  if (estSymétrique) {
    cat("La matrice est symétrique.\n")
  } else {
    cat("La matrice n'est pas symétrique.\n")
  }
  
  return(estSymétrique)
}

matriceAntisymetrique <- function(MatriceM){
  
}

matriceTransitive <- function(MatriceM){}

############################### relation R ###############################
#réflexive oui
matriceReflexive(matriceR)
#symétrique: OUI
TransposéeR <- transposeDUneMatrice(matriceR)
matriceR
TransposéeR
#ou
matriceSymétrique(matriceR)
#antisymétrique: non




############################### relation S ###############################



############################### relation T ###############################


##########################################################################
############################### Exercice 12 ##############################
##########################################################################

##########################################################################
############################### Exercice 13 ##############################
##########################################################################


















