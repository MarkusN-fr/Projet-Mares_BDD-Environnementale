library(tidyverse)

dat <- read.table("bdd_vf.txt", sep="\t", head=T, na.strings = "NC", row.names=1, dec=",")

load("Travail_MrMargerie/Workspace 16 04.RData")
# On s'intéresse ici à bddw32, qui servira de référence


# On vire les variables non utilisées par la promo précédente
# Seul problème : eux ils ont des données qu'on n'a pas ...

dat <- as.tibble(dat)

dat2 <- dat %>%
  select(TYPE = type,
         FORME = forme,
         TAIL_LONG = Longueur,
         TAIL_LARG = largeur,
         CONTEXTE = contexte_1,
         TOPO = topo,
         LIAI_HYDRO = liaison_reseau,
         ORIG_EAU = alim_specifique,
         REG_HYDRO = regime,
         EAU = turbidite,
         #DISTANCE = ?
         PROF_TO = profondeur,
         #PROF_MAX = ?
         PENTEDOUC = berges,
         PIETINEMEN = pietinement,
         BOISE = boisement,
         OMBRAGE = ombrage,
         HELOPHYTE = helophytes,
         HYDROPHYTR = hydrophytes_enracines,
         HYDRO2 = hydrophytes_non_enracines,
         #HYDRO3 = ?
         #HYDRO4 = ?
         ALGUES = algues,
         EAUHYDRO = fond_exonde,
         EAU_LIBRE = eau_libre,
         EVOL_MARE = evolution,
         POISSONS = poisson,
         AMPHIBIENS = amphibien,
         ODONATES = odonate,
         INVASIVES = presence_flore_exotique,
         #POT_ECO = ?
         Fond = nature_fond)

# On fait alors en sorte d'avoir les mêmes modalités de variables partout :

dat3 <- dat2 %>%
  transmute(
    
  )




