sardou <- read.table("bddw1.txt", head=T, na.strings="NA", row.names=1, dec=",")
#summary(sardou)

sardou2 <- sardou[,-2]

sardou2[which(sardou2$forme=="patatoide"), 33] <- "complexe"
sardou2[which(sardou2$forme=="triangle"), 33] <- "carre_rectangle"
sardou2$forme <- droplevels(sardou2$forme)

#Longueur
as.numeric(sardou2$L) -> Long
Long[Long<=30] <- 15
Long[Long>15&Long<=50] <- 30
Long[Long>50] <- 100
as.factor(Long) -> sardou$L

#Largeur
as.numeric(sardou2$l) -> Larg
Larg[Larg<=10] <- 5
Larg[Larg>5&Larg<=30] <- 20
Larg[Larg>20] <- 100
as.factor(Larg) -> sardou$l

#COntexte
as.character(sardou2$contexte1) -> con
con[con=="bas_marais_tourbiere_alcaline"] <- "tourbière"
con[con=="annexe_routiere_ferroviere"] <- "annexes_routieres"
con[con=="lande_humide"] <- "prairie_humide"
con[con=="prairie_humide"] <- "prairie_mesophile"
con[con=="jardin_parc_cour_de_ferme"] <- "parmi_habitations/milieux_urbains"
con[con=="bois_de_feuillus"] <- "bois_feuillus"

con[con=="fourres_bosquets"] <- "bois_de_feuillus"
con[con=="lande_humide"] <- "prairie_humide"


