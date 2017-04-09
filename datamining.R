# Data mining projet 
# M2 semestre 2
# -----------------------



library(plotly)
library(classInt)
library(ade4)
############################################
# Import des données
############################################
df = read.csv("Hopitaux.csv")
dff = df
boxplot(df[,2:ncol(df)], use.cols = T)

# Nombre de type d'établissement

nbe = length(unique(df[,1]))


# Which max trouve l'index de la valeur maximale de la colonne en paramètre,
# valeur potentiellement abbérante
# La commande which.max permet de trouver la valeur maximale, la plus éloignée vers le haut dans 
# le boxplot

# Supression de valeurs abérantes
# Supression de la 504

df2 = df[-which.max(x =df[,"AB"]),]

#Supression de la 527
df3 = df2[-which.max(x =df2[,"AN"]),]
df3 = df3[-which.max(x =df3[,"AN"]),]

df4 = df3[-which.max(x =df3[,"AC"]),]

df5 = df4[-which.max(x =df4[,"AS"]),]
df5 = df5[-which.max(x =df5[,"AI"]),]
df5 = df5[-which.max(x =df5[,"AI"]),]

# On sauvegarde puis on se debarasse de la colonne du type d'établissement
type_etab = as.data.frame( df5[,1])

df5 = df5[,-1]

# Observation du boxplot 

# boxplot(df5[,2:ncol(df2)], use.cols = T)

# for (i in 1:ncol(df5)){
#   
#   a[i] = classIntervals(df[,1],style = "hclust",n=9)
#   
# }

# p <- plot_ly(y = ~df5[,1], type = "box") 
# 
# for (j in 2:23){
#   p=p %>% add_trace(y = ~df5[,j])
#   
# }

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
nomvarint = matrix(0,ncol(df5),ncol(interv)-1)

for(i in 1:ncol(df5)){
  
  for(j in 1:ncol(interv)-1){
    
    nomvarint[i,j] = paste(interv[i,j],colnames(df5)[i],interv[i,j+1])
    
  }
}

nomvarint = matrix(nomvarint,1,9*23)

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

# On sauvegarde le tableau disjonctif creer et la matrice des intervalles
 # write.csv(tab1,"tableau_disjonctif_complet.csv")
 # write.csv(interv,"intervalles_variable.csv")

#On importe le tableau disjonctif

# disjonct = read.csv("tableau_disjonctif_complet.csv", header = T,sep = ";")


# ACM
# acm=dudi.acm(disjonct[,1:18])
# 
# scatter(acm)
# 
# s.label(acm$li)
# 
# s.arrow(acm$co)
# 
# plot(acm)

# ACM avec Factomine R

library(FactoMineR)

# ACM
res.mca = MCA(tab1)

#plot.MCA(res.mca,  invisible = "var",cex=0.7)
plotellipses(res.mca)

#CAH
res.hcpc = HCPC(res.mca)

# exportation des coordonnées de individus après l'ACM projection sur dim1 et dim2
write.csv(cbind(res.mca$ind$coord[,1:2],type_etab),"coord_indiv.csv")
