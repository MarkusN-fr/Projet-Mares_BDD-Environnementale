library(tidyverse)

dat <- read.table("bdd_vf.txt", sep="\t", head=T, na.strings = "NC", row.names=1, dec=",")

load("Travail_MrMargerie/Workspace 16 04.RData")
# On s'intéresse ici à bddw32, qui servira de référence


# On vire les variables non utilisées par la promo précédente
# Seul problème : eux ils ont des données qu'on n'a pas ...

id.numbers <- rownames(dat)
dat <- as.tibble(dat)

dat2 <- dat %>%
  add_column(ID = id.numbers) %>%
  select(ID = ID,
         TYPE = type,
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
  mutate(
    TYPE = fct_recode(TYPE,
                      "BASSIN" = "bassin",
                      "MARE" = "mare"),
    FORME = fct_recode(FORME,
                      "carre/rectangle" = "carre_rectangle",
                      "carre/rectangle" = "triangle",
                      "complexe" = "complexe",
                      "complexe" = "patatoide",
                      "ronde/ovale" = "ronde_ovale"),
    CONTEXTE = fct_recode(CONTEXTE,
                          "annexes_routieres" = "annexe_routiere_ferroviere",
                          "bois_feuillus" = "bois_de_feuillus",
                          "bois_feuillus" = "fourres_bosquets",
                          "culture" = "culture",
                          "parmi_habitations/milieux_urbains" = "jardin_parc_cour_de_ferme",
                          "prairie_mesophile" = "prairie_humide",
                          "prairie_mesophile" = "lande_humide",
                          "tourbière" = "bas_marais_tourbiere_alcaline",
                          "je_ne_sais_pas" = "indetermine"),
    TOPO = fct_recode(TOPO,
                      "fond_vallee" = "fond_de_vallee"),
    LIAI_HYDRO = fct_recode(LIAI_HYDRO,
                            "ecoulement_a_sec_en_sortie" = ,
                            "ecoulement_actif_en_sortie" = ,
                            "jamais_decoulement_en_sortie" = ),
  )




