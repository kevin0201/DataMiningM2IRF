# Data mining projet 
# M2 semestre 2

#sthda.com/mca

library(plotly)
library(classInt)
#library(ade4)
############################################
# Import des données
############################################

df = read.csv("Hopitaux.csv")
boxplot(df[,2:ncol(df)], use.cols = T)

# Nombre de type d'établissement

nbe = length(unique(df[,1]))

# Which max trouve l'index de la valeur maximale de la colonne en paramètre,
# valeur potentiellement abbérante
# La commande which.max permet de trouver la valeur maximale, la plus éloignée vers le haut dans 
# le boxplot

# Supression de valeurs abérantes
# Supression de la 504

df5 = df[-which.max(x = df[, "AB"]),][-which.max(x = df[, "AN"]),][-which.max(x = df[, "AN"]),][-which.max(x = df[, "AC"]),][-which.max(x = df[, "AS"]),][-which.max(x = df[, "AI"]),][-which.max(x = df[, "AI"]),]
df5typeeta = df5[, 1]
df5 = df5[,-1]

#  Nombre d'intervalles nint = 9
nint=9

# Methode de calcul des intervales

methodeinterv = "hclust"

# Matrice qui contiendra les bornes des intervals

interv=matrix(0,nrow = ncol(df5),ncol=nint+1)

# On calcul les intervalles, et on stock les bornes
# Pour chaque ligne on a les bornes d'intervalles une variable


for (i in 1:ncol(df5)){
  
  interv[i,] = classIntervals(df5[,i],style = methodeinterv,n=nint)$brks

}

# Creation des noms de la colonne de la matrice disjonctif
# Les nom sont de la forme [borne inf nom_variable borne supp]
nomvarint = matrix(0, nrow = ncol(df5), ncol =ncol(interv) - 1)

for(i in 1:ncol(df5)){
  
  for(j in 1:ncol(interv)-1){
    
    nomvarint[i,j] = paste(interv[i,j],colnames(df5)[i],interv[i,j+1])
    
  }
}

nomvarint = as.vector(t(nomvarint))

# Construction du tableau disjonctif complet
#  < 5 ">
# tab_disj_comp = matrix(0,nrow = nrow(df5),ncol = nint)

tab_disj_comp = array(data = "Non", dim = c(nrow(df5),nint,ncol(df5)), dimnames = NULL)

for (k in 1:ncol(df5))
{
    for (l in 1:nrow(df5))
    {
          for (f in 1:nint)
          {
                if(df5[l,k]>=interv[k,f] && df5[l,k]<interv[k,f+1])
                {
                    tab_disj_comp[l,f,k]="Oui"
                }
          }
    }
}

tab1 = matrix(tab_disj_comp[1:nrow(df5),1:nint,1],ncol = nint,nrow = nrow(df5))

for (z in 2:dim(tab_disj_comp)[3]){
  
  tab1 = cbind(tab1,matrix(tab_disj_comp[1:nrow(df5),1:nint,z],ncol = nint,nrow = nrow(df5)))
  
}

# tab1 est le tableau disjoctif complet
# On change les noms de colonnes du tableau disjonctif complet
tab1 = `colnames<-`(tab1,nomvarint)

#---------------------------
# ACM avec Factomine R

library(FactoMineR)
library(factoextra)
# ACM
res.mca = MCA(tab1, graph = FALSE, ncp = 5)

# plot variance bring per axe
fviz_screeplot(res.mca)

# plot variable + indi
fviz_mca_biplot(res.mca)

# plot indiv only
fviz_mca_ind(res.mca)

# plot variable only
fviz_mca_var(res.mca)

#plot.MCA(res.mca,  invisible = "var",cex=0.7)
#plotellipses(res.mca)

#CAH
res.hcpc = HCPC(res.mca)

# exportation des coordonnées de individus après l'ACM projection sur dim1 et dim2
# write.csv(cbind(res.mca$ind$coord[,1:2],type_etab),"coord_indiv.csv")

#----------------------------------------------
# Prediction sur les données brut init
library(rpart)
library(party)


## Create a formula for a model with a large number of variables:
# formule : prediction du type d'établissement en fonction des autres variables quantitaves
# application du dataframe initiale
fmla <- as.formula(paste("Type.Etablissement ~ ", paste(colnames(df5), collapse = "+")))

# creation de l'arbre à partir, formule, et données
type_eta = ctree(fmla, data = dff)

print(type_eta)

# plot de l'arbre de décision
plot(type_eta)

plot(type_eta, type = "simple")

# prédiction à partir de l'arbre de prédiction

table(predict(type_eta), dff$Type.Etablissement)

#----------------------------------
# Prédiction sur les données de acm
acm_ind_coord = data.frame(res.mca$ind$coord)

# On change les noms des colonnes pour pouvoir creer une formule pour la prédiction

acm_ind_coord = `colnames<-`(acm_ind_coord, c('a', 'b', 'c', 'd', 'e'))

# On colle les types d'établissement au tableau des coordonnées des ind à l'issue du mca pour generer l'arbre de décision
acm_ind_coord['Type.Etablissement'] = df5typeeta

# On cré la formule qui va prédire le type d'établissement en fonction des coordonnées fournies par le mca
(fmla <- as.formula(paste("Type.Etablissement ~ ", paste(c('a', 'b', 'c', 'd', 'e'), collapse = "+"))))

# creation de l'arbre à partir, formule, et données
type_eta_mca = ctree(fmla, data = acm_ind_coord)

print(type_eta_mca)

# plot de l'arbre de décision
plot(type_eta_mca)

plot(type_eta_mca, type = "simple")

# prédiction à partir de l'arbre de prédiction
df5$Type.Etablissement = df5typeeta
table(predict(type_eta_mca), df5$Type.Etablissement)
