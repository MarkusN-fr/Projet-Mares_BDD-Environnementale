
###BDD 2017

read.table("bdd_vf.txt", sep="\t" , head=T, na.strings = "NC", row.names=1, dec=",")->sardou
sardou2<-sardou[,-2]

#forme
sardou$forme->sardouf
sardouf[sardouf=="patatoide"]<-"complexe"
sardouf[sardouf=="triangle"]<-"carre_rectangle"
sardouf<- droplevels(sardouf)
as.factor(sardouf)->sardou2$forme

#Longueur
as.numeric(sardou$Longueur)->long
long[long<=30]<-15
long[long>15&long<=50]<-30
long[long>50]<-100
as.factor(long)->sardou2$Longueur

#Largeur
as.numeric(sardou$largeur)->larg
larg[larg<=10]<-5
larg[larg>5&larg<=30]<-20
larg[larg>20]<-100
as.factor(larg)->sardou2$largeur

#Contexte
as.character(sardou$contexte_1)->cont
cont[cont=="fourres_bosquets"]<-"bois_de_feuillus"
cont[cont=="bas_marais_tourbiere_alcaline"]<-"tourbière"
cont[cont=="fourres_humide"]<-"prairie_humide"
cont[cont=="prairie_humide"]<-"prairie_mesophile"
cont[cont=="jardin_parc_cour_de_ferme"]<-"parmi_habitations/milieux_urbains"
cont[cont=="bois_de_feuillus"]<-"bois_feuillus"
cont[cont=="annexe_routiere_ferroviere"]<-"annexes_routieres"
as.factor(cont)->sardou2$contexte_1


# TOPO , rien à changer
as.factor(sardou$topo)->sardou2$topo


# Liai_hydro (Liaison avec le réseau hydrographique superficiel) , pas les mêmes modalités que pour nous 


# Orig_eau, pas d'équivalent ou pas les mêmes modalités


# Régime hydrologique
as.character(sardou$regime)->reg
reg[reg=="mare_permanente"]<-"permanente"
reg[reg=="mare_temporaire"]<-"temporaire"
reg[reg=="indeterminee"]<-"temporaire"
as.factor(reg)->sardou2$regime

# Turbidité , rien à changer (équivalent de "eau" pour 2016)
as.factor(sardou$turbidite)->sardou2$turbidite


# Distance (de la voie publique), pas d'équivalent


# Profondeur (équivalent prof_to)
as.numeric(sardou$profondeur)->prof
prof[prof<=30]<-15
prof[prof>30&prof<=65]<-50
prof[prof>65]<-100
as.factor(prof)->sardou2$profondeur


# Prof_max (profondeur maximum évaluée), pas d'equivalent


# Berges (équivalent pentedouc 2016)
as.numeric(sardou$berges)->berge
berge[berge<=18]<-10
berge[berge>10&berge<=55]<-30
berge[berge>55]<-60
as.factor(berge)->sardou2$berges


# Pietinement
as.character(sardou$pietinement)->piet
piet[piet=="localise"]<-"intense_et_localise"
as.factor(piet)->sardou2$pietinement


# Boisement (aux abords)
as.numeric(sardou$boisement)->bois
bois[bois<=20]<-10
bois[bois>20&bois<=55]<-40
bois[bois>55]<-100
as.factor(bois)->sardou2$boisement


# Ombrage
as.numeric(sardou$ombrage)->ombre
ombre[ombre<=20]<-10
ombre[ombre>20&ombre<=55]<-40
ombre[ombre>55]<-100
as.factor(ombre)->sardou2$ombrage


# Helophytes
as.numeric(sardou$helophytes)->helo
helo[helo<=10]<-5
helo[helo>10&helo<=55]<-30
helo[helo>55]<-75
as.factor(helo)->sardou2$helophytes

# hydrophytes enracinées
as.numeric(sardou$hydrophytes_enracines)->hydro1
hydro1[hydro1<=10]<-5
hydro1[hydro1>10&hydro1<=55]<-30
hydro1[hydro1>55]<-75
as.factor(hydro1)->sardou2$hydrophytes_enracines
# Correspond à hydrophytr + hydro 2 + hydro3 + eauhydro


# hydrophytes non enracinées = hydro 4
as.numeric(sardou$hydrophytes_non_enracines)->hydro2
hydro2[hydro2<=10]<-5
hydro2[hydro2>10&hydro2<=55]<-30
hydro2[hydro2>55]<-75
as.factor(hydro2)->sardou2$hydrophytes_non_enracines


# Algues
as.numeric(sardou$algues)->alg
alg[alg<=10]<-5
alg[alg>10&alg<=55]<-30
alg[alg>55]<-75
as.factor(alg)->sardou2$algues


# Eau libre 
as.numeric(sardou$eau_libre)->eaul
eaul[eaul<=20]<-10
eaul[eaul>10&eaul<=50]<-30
eaul[eaul>50&eaul<=80]<-65
eaul[eaul>80]<-90
as.factor(eaul)->sardou2$eau_libre


# Evolution mare, rien à changer
as.factor(sardou$evolution)->sardou2$evolution


# Poissons
as.character(sardou$poisson)->poi
poi[poi=="non"]<-"probablement_pas"
as.factor(poi)->sardou2$poisson


# Amphibiens , rien à changer
as.factor(sardou$amphibien)->sardou2$amphibien


# Odonates, rien à changer
as.factor(sardou$odonate)->sardou2$odonate


# Invasive faune et flore, non ou NA partout, utilisation de celle avec le plus de données = faune
as.factor(sardou$presence_faune_exotique)->sardou2$presence_faune_exotique


#Pot_eco, pas d'équivalent


# Fond
as.character(sardou$nature_fond)->fond
fond[fond=="bache"]<-"artificiel"
as.factor(fond)->sardou2$nature_fond


# On garde que les colonnes qui nous intéressent et on le met e forme pour le comparer
bdd2017w1<-sardou2[,-c(1,2,3,4,6,7,8,9,10,12,14,15,17,18,19,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,38,39,40,41,48,51,52,59,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77)]
bdd2017<-data.frame(TYPE=bdd2017w1$type,
                    FORME=bdd2017w1$forme,
                    TAIL_LONG=bdd2017w1$Longueur,
                    TAIL_LARG=bdd2017w1$largeur,
                    CONTEXTE=bdd2017w1$contexte_1,
                    TOPO=bdd2017w1$topo,
                    REG_HYDRO=bdd2017w1$regime,
                    EAU=bdd2017w1$turbidite,
                    PROF_TO=bdd2017w1$profondeur,
                    PENTEDOUC=bdd2017w1$berges,
                    PIETINEMEN=bdd2017w1$pietinement,
                    BOISE=bdd2017w1$boisement,
                    OMBRAGE=bdd2017w1$ombrage,
                    HELOPHYTE=bdd2017w1$helophytes,
                    HYDROPHYTR=bdd2017w1$hydrophytes_enracines,
                    HYDRO4=bdd2017w1$hydrophytes_non_enracines,
                    ALGUES=bdd2017w1$algues,
                    EAU_LIBRE=bdd2017w1$eau_libre,
                    EVOL_MARE=bdd2017w1$evolution,
                    POISSONS=bdd2017w1$poisson,
                    AMPHIBIENS=bdd2017w1$amphibien,
                    ODONATES=bdd2017w1$odonate,
                    INVASIVES=bdd2017w1$presence_faune_exotique,
                    Fond=bdd2017w1$nature_fond)


################# Fin de la mise en forme de données 2017 #################


# Ouverture du fichier "Workspace 16 04.RData"
# Mise en forme bddw32 pour comparaison hydrophyte

hydro2016_1<-as.numeric(bddw32$HYDROPHYTR)
hydro2016_1[hydro2016_1==1]<-5
hydro2016_1[hydro2016_1==2]<-30
hydro2016_1[hydro2016_1==3]<-75

hydro2016_2<-as.numeric(bddw32$HYDRO2)
hydro2016_2[hydro2016_2==1]<-5
hydro2016_2[hydro2016_2==2]<-30
hydro2016_2[hydro2016_2==3]<-75

hydro2016_3<-as.numeric(bddw32$HYDRO3)
hydro2016_3[hydro2016_3==1]<-5
hydro2016_3[hydro2016_3==2]<-30
hydro2016_3[hydro2016_3==3]<-75

hydro2016_4<-as.numeric(bddw32$EAUHYDRO)
hydro2016_4[hydro2016_4==1]<-5
hydro2016_4[hydro2016_4==2]<-30
hydro2016_4[hydro2016_4==3]<-75

hydro2016<-hydro2016_1+hydro2016_2+hydro2016_3+hydro2016_4
hydro2016[hydro2016<=20]<-5
hydro2016[hydro2016>20&hydro2016<=70]<-30
hydro2016[hydro2016>70]<-75
as.factor(hydro2016)->bddw32$HYDROPHYTR



#On garde que les colonnes qui nous intéressent pour 2016
bdd2016<-bddw32[,-c(7,8,11,13,20,21,24,31)]



################### Comparaison ###################

# Tableaux à utiliser : bdd2016 et bdd2017

