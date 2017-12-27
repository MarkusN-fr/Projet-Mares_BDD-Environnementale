library(tidyverse)

dat <- read.table("bdd_vf.txt", sep="\t", head=T, na.strings = "NC", row.names=1, dec=",")

id.numbers <- rownames(dat)
dat <- as.tibble(dat)

dat2 <- dat %>%
  add_column(ID = id.numbers) %>%
  select(ID,
         Type = type,
         Forme = forme,
         Longueur,
         Largeur = largeur,
         Contexte = contexte_1,
         Topo = topo,
         Liaison_Hydro = liaison_reseau,
         Alim_Spe = alim_specifique,
         Regime = regime,
         Turbidite = turbidite,
         Profondeur = profondeur,
         Berges = berges,
         Pietinement = pietinement,
         Boisement = boisement,
         Ombrage = ombrage,
         Helophytes = helophytes,
         Hydrophytes_Enr = hydrophytes_enracines,
         Hydrophytes_NEnr = hydrophytes_non_enracines,
         Algues = algues,
         Fond_Ex = fond_exonde,
         Eau_Libre = eau_libre,
         Evolution = evolution,
         Poissons = poisson,
         Amphibiens = amphibien,
         Odonates = odonate,
         Invasives = presence_flore_exotique,
         Fond = nature_fond,
         Commentaire = commentaire) %>%
  filter(Commentaire != "mare_comblee" | is.na(Commentaire)) %>%
  filter(Commentaire != "mare_seche" | is.na(Commentaire)) %>%
  filter(Commentaire != "mare_seche_ou_pas_trouvee" | is.na(Commentaire)) %>%
  filter(!is.na(Forme)) %>%
  filter(!is.na(Longueur))

ggplot(dat2, aes(x = Contexte)) + geom_bar(aes(y = Fond_Ex), stat = "identity")


# Test pour représenter le recouvrement en végétation selon le milieu

rec <- dat2 %>%
  select(Contexte,
         Evolution,
         Profondeur,
         Topo,
         Helophytes,
         Hydrophytes_Enr,
         Hydrophytes_NEnr,
         Algues,
         Fond_Ex,
         Eau_Libre)

rec2 <- rec %>%
  gather(Helophytes,
         Hydrophytes_Enr,
         Hydrophytes_NEnr,
         Algues,
         Fond_Ex,
         Eau_Libre,
         key = "Vegetation",
         value = "Recouvrement")
rec2 <- rec2 %>%
  mutate(
    Vegetation = fct_relevel(as.factor(Vegetation), c("Helophytes", "Hydrophytes_Enr", "Hydrophytes_NEnr", 
                                                      "Algues", "Eau_Libre", "Fond_Ex"))
  )

Recouvrement <- ggplot(rec2) + 
  geom_bar(aes(x = Evolution, y = Recouvrement, fill = Vegetation), stat = "identity", position = "fill") +
  ggtitle("Recouvrement des mares selon le stade d'évolution et le type de Végétation") +
  theme(plot.title = element_text(hjust = .5, size = 24))
Recouvrement

Prof <- ggplot(rec2, aes(x = Profondeur, y = Recouvrement, col = Vegetation)) +
  geom_point() +
  geom_smooth(se = F)
Prof















