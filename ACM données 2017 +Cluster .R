## Analyse données 2017


### Objectif : analyse multivarié et voir l'intérêt de chaque variable et faire des groupes



## Pour cela plusieurs étapes :

################## Etape 1 : Préparation pour l'ACM ##################

## Utilisation du code fait pour la comparaison, compléter avec nos variables

read.table("bdd_vf.txt", sep="\t" , head=T, na.strings = "NC", row.names=1, dec=",")->enviro
enviro2<-enviro[,-c(1,2,3,4,5,8,9,10,11,78)]


#Type, RAC (rien à changer)
as.factor(enviro$type)->enviro2$type


#Statut, RAC
as.factor(enviro$statut)->enviro2$statut


# Amphibien, RAC
as.factor(enviro$amphibien)->enviro2$amphibien


# Odonates, RAC
as.factor(enviro$odonate)->enviro2$odonate


# Invertébrés, RAC
as.factor(enviro$invertebre.aquatique)->enviro2$invertebre.aquatique


#Faune autre, RAC
as.factor(enviro$faune_autre)->enviro2$faune_autre


# Poissons
as.factor(enviro$poisson)->enviro2$poisson


# Anatidés, RAC
as.factor(enviro$anatide)->enviro2$anatide


# Pas de faune, RAC
as.factor(enviro$faune_aucun)->enviro2$faune_aucun


# Type de mare, RAC
as.factor(enviro$type_de_mare)->enviro2$type_de_mare


# Evolution mare, RAC
as.factor(enviro$evolution)->enviro2$evolution


# Partie usage

# Abreuvoir aménagé, RAC
as.factor(enviro$abreuvoir_amenage)->enviro2$abreuvoir_amenage

# Abreuvor non aménagé, RAC
as.factor(enviro$abreuvoir_non_amenage)->enviro2$abreuvoir_non_amenage

#Collecte ruisselement, RAC
as.factor(enviro$collecte_ruissellement)->enviro2$collecte_ruissellement

# Peche, RAC
as.factor(enviro$peche)->enviro2$peche

# Chasse, RAC
as.factor(enviro$chasse)->enviro2$chasse

# Reserve incendie, RAC
as.factor(enviro$reserve_incendie)->enviro2$reserve_incendie

# Ornemenetale, RAC
as.factor(enviro$ornemental)->enviro2$ornemental

# Biodiversité
as.factor(enviro$biodiversite)->enviro2$biodiversite

# Patrimoine
as.factor(enviro$patrimoine)->enviro2$patrimoine

# Pédagogique
as.factor(enviro$pedagogique)->enviro2$pedagogique

# Abandonnée
as.factor(enviro$abandonne)->enviro2$abandonne

# Lagunage
as.factor(enviro$lagunage)->enviro2$lagunage

# Usage inconnu
as.factor(enviro$usage_inconnu)->enviro2$usage_inconnu

# Pompe à nez
as.factor(enviro$pompe_a_nez)->enviro2$pompe_a_nez


# Dechets
as.character(enviro$dechets)->dech
dech[dech=="non"]<-"aucun"
dech[dech=="dechets_recyclables_dechets_inertes"]<-"dechets_recyclables"
dech[dech=="dechets_verts_ordures_menageres"]<-"ordures_menageres"
dech[dech=="ordures_menageres_dechets_recyclables"]<-"ordures_menageres"
as.factor(dech)->enviro2$dechets


# TOPO , RAC
as.factor(enviro$topo)->enviro2$topo


# Contexte 1
as.character(enviro$contexte_1)->cont
cont[cont=="fourres_bosquets"]<-"bois_de_feuillus"
cont[cont=="bas_marais_tourbiere_alcaline"]<-"tourbière"
cont[cont=="fourres_humide"]<-"prairie_humide"
cont[cont=="prairie_humide"]<-"prairie_mesophile"
cont[cont=="jardin_parc_cour_de_ferme"]<-"parmi_habitations/milieux_urbains"
cont[cont=="bois_de_feuillus"]<-"bois_feuillus"
cont[cont=="annexe_routiere_ferroviere"]<-"annexes_routieres"
as.factor(cont)->enviro2$contexte_1


# Contexte 2
as.factor(enviro$contexte_2)->enviro2$contexte_2


# Petit patrimoine
as.factor(enviro$petit_patrimoine_bati)->enviro2$petit_patrimoine_bati


# Cloture
as.factor(enviro$cloture)->enviro2$cloture


# Haie
as.factor(enviro$haie)->enviro2$haie
          
          

#forme
enviro$forme->envirof
envirof[envirof=="patatoide"]<-"complexe"
envirof[envirof=="triangle"]<-"carre_rectangle"
envirof<- droplevels(envirof)
as.factor(envirof)->enviro2$forme

#Longueur
as.numeric(enviro$Longueur)->long
long[long<=30]<-15
long[long>15&long<=50]<-30
long[long>50]<-100
as.factor(long)->enviro2$Longueur

#Largeur
as.numeric(enviro$largeur)->larg
larg[larg<=10]<-5
larg[larg>5&larg<=30]<-20
larg[larg>20]<-100
as.factor(larg)->enviro2$largeur


# Profondeur
as.numeric(enviro$profondeur)->prof
prof[prof<=30]<-15
prof[prof>30&prof<=65]<-50
prof[prof>65]<-100
as.factor(prof)->enviro2$profondeur


# Nature fond
as.character(enviro$nature_fond)->fond
fond[fond=="bache"]<-"artificiel"
as.factor(fond)->enviro2$nature_fond


# Berges 
as.numeric(enviro$berges)->berge
berge[berge<=18]<-10
berge[berge>10&berge<=55]<-30
berge[berge>55]<-60
as.factor(berge)->enviro2$berges


# Bourrelet
as.factor(enviro$bourrelet)->enviro2$bourrelet


# Pietinement
as.character(enviro$pietinement)->piet
piet[piet=="localise"]<-"intense_et_localise"
as.factor(piet)->enviro2$pietinement


# Régime hydrologique
as.character(enviro$regime)->reg
reg[reg=="mare_permanente"]<-"permanente"
reg[reg=="mare_temporaire"]<-"temporaire"
reg[reg=="indeterminee"]<-"temporaire"
as.factor(reg)->enviro2$regime


# Liaison réseau 
as.character(enviro$liaison_reseau)->res
res[res=="inconnu"]<-"aucun"
res[res=="cours_d_eau"]<-"axe_de_ruissellement"
res[res=="drainage_pompage"]<-"autre"
as.factor(res)->enviro2$liaison_reseau


# Alimentation spécifique
as.character(enviro$alim_specifique)->alim
alim[alim=="indetermine"]<-"aucune"
alim[alim=="ruissellement_culture"]<-"ruissellement"
alim[alim=="ruissellement_voierie"]<-"ruissellement"
alim[alim=="pluvial_bati"]<-"autre"
alim[alim=="source"]<-"nappe"
as.factor(alim)->enviro2$alim_specifique


# Turbidité , RAC
as.factor(enviro$turbidite)->enviro2$turbidite


# Helophytes
as.numeric(enviro$helophytes)->helo
helo[helo<=10]<-5
helo[helo>10&helo<=55]<-30
helo[helo>55]<-75
as.factor(helo)->enviro2$helophytes


# hydrophytes enracinées
as.numeric(enviro$hydrophytes_enracines)->hydro1
hydro1[hydro1<=10]<-5
hydro1[hydro1>10&hydro1<=55]<-30
hydro1[hydro1>55]<-75
as.factor(hydro1)->enviro2$hydrophytes_enracines


# hydrophytes non enracinées
as.numeric(enviro$hydrophytes_non_enracines)->hydro2
hydro2[hydro2<=10]<-5
hydro2[hydro2>10&hydro2<=55]<-30
hydro2[hydro2>55]<-75
as.factor(hydro2)->enviro2$hydrophytes_non_enracines


# Algues
as.numeric(enviro$algues)->alg
alg[alg<=10]<-5
alg[alg>10&alg<=55]<-30
alg[alg>55]<-75
as.factor(alg)->enviro2$algues


# Eau libre 
as.numeric(enviro$eau_libre)->eaul
eaul[eaul<=20]<-10
eaul[eaul>10&eaul<=50]<-30
eaul[eaul>50&eaul<=80]<-65
eaul[eaul>80]<-90
as.factor(eaul)->enviro2$eau_libre


# Fond exonde
as.numeric(enviro$fond_exonde)->exon
exon[exon<=5]<-0
exon[exon>5&exon<=55]<-30
exon[exon>55]<-75
as.factor(exon)->enviro2$fond_exonde


# Boisement 
as.numeric(enviro$boisement)->bois
bois[bois<=20]<-10
bois[bois>20&bois<=55]<-40
bois[bois>55]<-100
as.factor(bois)->enviro2$boisement


# Ombrage
as.numeric(enviro$ombrage)->ombre
ombre[ombre<=20]<-10
ombre[ombre>20&ombre<=55]<-40
ombre[ombre>55]<-100
as.factor(ombre)->enviro2$ombrage


# Faune exotique
as.factor(enviro$presence_faune_exotique)->enviro2$presence_faune_exotique

# Flore exotique
as.factor(enviro$presence_flore_exotique)->enviro2$presence_flore_exotique

# Partie travaux

# Aucun travaux 
as.factor(enviro$travaux_aucun)->enviro2$travaux_aucun

# Curage
as.factor(enviro$curage)->enviro2$curage

# Profilage berge
as.factor(enviro$profilage_berge)->enviro2$profilage_berge

# Bucheronnage
as.factor(enviro$bucheronnage)->enviro2$bucheronnage

# Debrousaillage
as.factor(enviro$debroussaillage)->enviro2$debroussaillage

# Pose cloture
as.factor(enviro$pose_de_cloture)->enviro2$pose_de_cloture

# Abreuvoir
as.factor(enviro$abreuvoir)->enviro2$abreuvoir

# Lutte contre EEE
as.factor(enviro$lutte_contre_EEE)->enviro2$lutte_contre_EEE

# Netoyage dechets
as.factor(enviro$nettoyage_dechets)->enviro2$nettoyage_dechets

# Arrachage
as.factor(enviro$arrachage)->enviro2$arrachage

# Fonctionnement hydraulique
as.factor(enviro$fonctionnement_hydraulique)->enviro2$fonctionnement_hydraulique

# Fauchage tardif
as.factor(enviro$fauchage.tardif)->enviro2$fauchage.tardif

# Travaux autre
as.factor(enviro$travaux_autre)->enviro2$travaux_autre


################### Fin mise en forme ######################


### Etape 2 : ACM
library(ade4)
dudi.acm(enviro2)->acm_enviro
scatter(acm_enviro)

#Suppression variable à une modalité 
enviro2[,-c(4,9,16,17,18,22,24,26,30,54,55,59,66,67)]->enviro3

dudi.acm(enviro3)->acmw1
scatter(acmw1)



### Etape 3 : Faire des groupes

plot(hclust(dist(acmw1$li)))
cut4<- cutree(hclust(dist(acmw1$li)), k=4)
s.class(acmw1$li, factor(cut4))

# Verifier si on a choisi le bon nombre de groupe
library(cluster)
pam(acmw1$li, k=4)->pam4
plot(pam4) # 0,57 ; 2 négatifs
pam(acmw1$li, k=3)->pam3
plot(pam3) # 0,57 ; 4 négatifs
pam(acmw1$li, k=2)->pam2
plot(pam2) # 0,54 ; 0 negatfs
pam(acmw1$li, k=5)->pam5
plot(pam5) # 0,57 ; 2 négatifs
pam(acmw1$li, k=6)->pam6
plot(pam6) # 0,57 ; 4 négatifs
# 4 Clusters semble un bon compromis


# Impact d'un facteur sur la mise en place des cluster
chisq.test(pam4$clustering, enviro3[,1])

#Regarder quelles mares apartiennent à quel cluster
acmw1$li[pam4$clustering=="1",]->clust1 #Isole les mares appartenant au cluster 1


