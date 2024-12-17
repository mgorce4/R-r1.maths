##########################################################################
## Installation de package et chargement du package, si nécessaire #######
##########################################################################
# if (!require("matlib")) {
#   install.packages("matlib")  
# }
# library(matlib)

##########################################################################
############################### Exercice 1  ##############################
##########################################################################

creationEnsemble <- function(...) {
  #me récupère les arguments de la liste ...
  args <- list(...)
  
  if (length(args) == 0) {
    return(NULL)  # Renvoie un ensemble vide si aucun argument
  }
  
  # Combine tous les arguments dans un seul vecteur
  combined <- unlist(args)
  
  return(unique(combined))
}

ensemble1 <- creationEnsemble(1,2,3,4)
ensemble1 
ensemble2 <- creationEnsemble("A","B", "C", "a","b", "c")
ensemble2
ensemble3 <- creationEnsemble()
ensemble3
ensemble4 <- creationEnsemble(1:4, 1:4, 5,5,6)
ensemble4


##########################################################################
############################### Exercice 2  ##############################
##########################################################################

unionDeDeuxEnsembles <- function(ensembleA, ensembleB){
  # Combine both ensembles using concatenation
  combine <- c(ensembleA, ensembleB)
  
  # Retirer les doublons pour obtenir l'union des deux ensembles
  return(unique(combine))
}

ensemble1Et1 <- unionDeDeuxEnsembles(ensembleA = ensemble1, ensembleB = ensemble1)
ensemble1Et2 <- unionDeDeuxEnsembles(ensembleA = ensemble1, ensembleB = ensemble2)
ensemble1Et3 <- unionDeDeuxEnsembles(ensembleA = ensemble1, ensembleB = ensemble3)
ensemble1Et4 <- unionDeDeuxEnsembles(ensembleA = ensemble1, ensembleB = ensemble4)
ensemble2Et2 <- unionDeDeuxEnsembles(ensembleA = ensemble2, ensembleB = ensemble2)
ensemble2Et3 <- unionDeDeuxEnsembles(ensembleA = ensemble2, ensembleB = ensemble3)
ensemble2Et4 <- unionDeDeuxEnsembles(ensembleA = ensemble2, ensembleB = ensemble4)
ensemble3Et3 <- unionDeDeuxEnsembles(ensembleA = ensemble3, ensembleB = ensemble3)
ensemble3Et4 <- unionDeDeuxEnsembles(ensembleA = ensemble3, ensembleB = ensemble4)
ensemble4Et4 <- unionDeDeuxEnsembles(ensembleA = ensemble4, ensembleB = ensemble4)
ensemble1Et1
ensemble1Et2
ensemble1Et3
ensemble1Et4
ensemble2Et2
ensemble2Et3
ensemble2Et4
ensemble3Et3
ensemble3Et4
ensemble4Et4
setequal(ensemble1Et2, union(x=ensemble1,y=ensemble2))
setequal(ensemble2Et4, union(x=ensemble2,y=ensemble4))

##########################################################################
############################### Exercice 3  ##############################
##########################################################################

intersectionDeDeuxEnsembles <- function(ensembleA, ensembleB){
  
  #garde seulement l'intersection et supprime les doublons
  resultat <- ensembleA[ensembleA %in% ensembleB]
  
  if (length(resultat) == 0) {
    return(NULL)  # Renvoie un ensemble vide si aucun argument
  }
  
  return(unique(resultat))
}


ensemble1112 <- intersectionDeDeuxEnsembles(ensembleA = ensemble1Et1, ensembleB = ensemble1Et2)
ensemble1222 <- intersectionDeDeuxEnsembles(ensembleA = ensemble1Et2, ensembleB = ensemble2Et2)
ensemble1112
ensemble1222
setequal(ensemble1112, intersect(x=ensemble1Et1, y=ensemble1Et2))
setequal(ensemble1222, intersect(x=ensemble1Et2, y=ensemble2Et2))
ensemble1112bis <- intersectionDeDeuxEnsembles(ensembleB = ensemble1Et1, ensembleA = ensemble1Et2)
setequal(ensemble1112bis, ensemble1112)

##########################################################################
############################### Exercice 4  ##############################
##########################################################################

differenceDeDeuxEnsembles <- function(ensembleA, ensembleB){
  
  # Garder les éléments de ensembleA qui ne sont pas dans ensembleB
  resultat <- ensembleA[!(ensembleA %in% ensembleB)]
  
  if (length(resultat) == 0) {
    return(NULL)  # Renvoie un ensemble vide si aucun argument
  }
  
  # Retirer les doublons
  return(unique(resultat))
}


ensemble11Diff12 <- differenceDeDeuxEnsembles(ensembleA = ensemble1Et1, ensembleB = ensemble1Et2)
ensemble12Diff11 <- differenceDeDeuxEnsembles(ensembleB = ensemble1Et1, ensembleA = ensemble1Et2)
ensemble11Diff14 <- differenceDeDeuxEnsembles(ensembleA = ensemble1Et1, ensembleB = ensemble1Et4)
ensemble24Diff11 <- differenceDeDeuxEnsembles(ensembleA = ensemble2Et4, ensembleB = ensemble1Et1)

ensemble11Diff12
ensemble12Diff11
ensemble11Diff14
ensemble24Diff11

setequal(ensemble11Diff12, setdiff(x= ensemble1Et1, y = ensemble1Et2))
setequal(ensemble12Diff11, setdiff(y= ensemble1Et1, x = ensemble1Et2))
setequal(ensemble11Diff14, setdiff(x= ensemble1Et1, y = ensemble1Et4))
setequal(ensemble24Diff11, setdiff(x= ensemble2Et4, y = ensemble1Et1))



differenceSymetriqueDeDeuxEnsembles <- function(ensembleC, ensembleD){
  # Obtenir les éléments qui sont dans ensembleA mais pas dans ensembleB
  differenceC <- ensembleC[!(ensembleC %in% ensembleD)]
  
  # Obtenir les éléments qui sont dans ensembleB mais pas dans ensembleA
  differenceD <- ensembleD[!(ensembleD %in% ensembleC)]
  
  # Combiner les deux différences et enlever les doublons
  resultat <- unique(c(differenceC, differenceD))
  
  return(resultat)
}


ensemble5 <- creationEnsemble(1:4,8:10)
ensemble5Diff24 <- differenceSymetriqueDeDeuxEnsembles(ensembleC = ensemble5, ensembleD = ensemble2Et4)
ensemble5Diff24

##########################################################################
############################### Exercice 5  ##############################
##########################################################################

complementaireDUnEnsemble <- function(ensembleE, ensembleF){
  
  resultat <- differenceDeDeuxEnsembles(ensembleA = ensembleF, ensembleB = ensembleE)
  inter <- intersectionDeDeuxEnsembles(ensembleA = ensembleE, ensembleB = ensembleF)
  
  if (length(inter) == 0) {
    return("Erreur le premier ensemble n'est pas inclus dans le second, il n'est pas possible de prendre le complémentaire du premier par rapport au second")
  }
  
   return(resultat)
}

ensemble11Comp24 <- complementaireDUnEnsemble(ensembleE= ensemble1Et1, ensembleF = ensemble2Et4)
ensemble11Comp24
ensemble11Comp22 <- complementaireDUnEnsemble(ensembleE=ensemble1Et1, ensembleF=ensemble2Et2)
ensemble11Comp22

##########################################################################
############################### Exercice 6  ##############################
##########################################################################

cardinalDUnEnsemble<- function(ensembleG){
  resultat<- length(ensembleG)
  
  return(resultat)
  
}

ensemble1
cardinalDUnEnsemble(ensemble1)
ensemble2
cardinalDUnEnsemble(ensemble2)
ensemble3
cardinalDUnEnsemble(ensemble3)
ensemble4
cardinalDUnEnsemble(ensemble4)

##########################################################################
############################### Exercice 7  ##############################
##########################################################################

produitCartesienDeDeuxEnsembles <- function(ensembleA, ensembleB) {
  # Vérifier que les deux ensembles sont définis
  if (missing(ensembleA) || missing(ensembleB)) {
    stop("Les deux ensembles doivent être spécifiés.")
  }
  
  # Calculer le produit cartésien
  produitCartesien <- expand.grid(ensembleA, ensembleB)
  
  # Renommer les colonnes pour une meilleure lisibilité
  colnames(produitCartesien) <- c("ElementA", "ElementB")
  
  return(produitCartesien)
}

ensembleProduit12 <- produitCartesienDeDeuxEnsembles(ensembleA = ensemble1, ensembleB = ensemble2)
ensembleProduit12bis <- expand.grid(ensemble1,ensemble2)


##########################################################################
############################### Exercice 10  #############################
##########################################################################

toutesLesPartiesDUnEnsemble <- function(ensemble) {
  
  if (length(ensemble) == 0) {
    return(list(numeric(0)))  # Si l'ensemble est vide, retourner une liste contenant l'ensemble vide
  }
  
  # Trouver toutes les combinaisons possibles
  parties <- lapply(0:length(ensemble), function(k) {
    combn(ensemble, k, simplify = FALSE)  # Génère des combinaisons de k éléments
  })
  
  # Combiner toutes les parties dans une seule liste
  ensembleDesParties <- unlist(parties, recursive = FALSE)
  
  return(ensembleDesParties)
}

toutesLesPartiesDUnEnsemble(creationEnsemble(2))

numeric(0)