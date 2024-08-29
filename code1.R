library(haven)
library(dplyr)
library(nnet)
library(stargazer)
library(foreign)

ECMOSS<-read_dta("/Users//Desktop/Ecmoss_2006.dta")

##########################################################################################
# Importation et nettoyage base
##########################################################################################

# we remove "pas de reponse", "ne sait pas" and NA wages

# we remove "pas de reponse", "ne sait pas" and NA wages
base<-ECMOSS[ECMOSS$pee !=0 & ECMOSS$pee !=3 & ECMOSS$pee !="E" &
               ECMOSS$interes !=0 & ECMOSS$interes !=3 & ECMOSS$interes !="E" &
               ECMOSS$njc != 2 &
               ECMOSS$cpfd == "C" &
               ECMOSS$njc > 359 &
               ECMOSS$age_r < 55 &
               ECMOSS$qs22_r == "Cdi-tit" &
               ECMOSS$qs24_r > 11 &
               ECMOSS$s_brut > 15000 &
               ECMOSS$s_brut < 150000 &
               ECMOSS$qs3_r > 15000 &
               ECMOSS$s_brut < 150000, ]

# remove NA
base<-base[!is.na (base$pee) &
             !is.na (base$interes) &
             !is.na(base$cpfd) &
             !is.na(base$s_brut) &
             !is.na(base$qs3_r) &
             !is.na(base$qs26_r) &
             !is.na(base$sexe_r) &
             !is.na(base$nes16_r) &
             !is.na(base$tefen) &
             !is.na(base$dept), ]

# Proposition pour simplifier le calcul plus tard : on remplace les NA par des 0

base[is.na(base)] <- 0

# transform as factors
is.factor(base$pee)
is.factor(base$interes)
base$pee <- as.factor(base$pee)
base$interes <- as.factor(base$interes)
is.numeric(base$s_brut)
is.numeric(base$qs3_r)
is.factor(base$siret)
base$siret <- as.factor(base$siret)


##########################################################################################
# Statistiques Descriptives
##########################################################################################

# Extraction des int??ressements  et plan d'??pargne par entreprises pour stats descriptives

sd_interes_pee <- select(base, siret, interes, pee)
sd_interes_pee <- na.exclude(sd_interes_pee)
sd_interes_pee$siret <- as.numeric(sd_interes_pee$siret)
sd_interes_pee$interes <- as.numeric(sd_interes_pee$interes)
sd_interes_pee$pee <- as.numeric(sd_interes_pee$pee)

#??limination des doublons dans un vecteur 
doublons_sd_interes_pee <- which(duplicated(sd_interes_pee$siret))
sd_interes_pee <- sd_interes_pee[-doublons_sd_interes_pee,] 

# Statistiques descriptives par entreprise

table(sd_interes_pee$interes)
table(sd_interes_pee$pee)

# Stat desc par employ??

table(base$interes)
table(base$plan_epargne)


##########################################################################################
# Recodage variables
##########################################################################################

# sal_base = total gross annual - (paid leave and overtime + bonuses + various supplement)
# sal_base = Qs3 - qs3.1 - qs3.2 - qs3.3

is.numeric(base$qs3_r)
is.numeric(base$qs3_1_r)
is.numeric(base$qs3_2_r)
is.numeric(base$qs3_3_r)

base$sal_base <- (base$qs3_r - base$qs3_1_r - base$qs3_2_r - base$qs3_3_r)
base$l_sal_base <- log10(base$sal_base)

#interes et pee

base$interessement <- 0
base$plan_epargne <- 0

base$interessement[base$interes ==1] <-1
base$interessement[base$interes ==2] <-0

base$plan_epargne[base$pee ==1] <-1
base$plan_epargne[base$pee ==2] <-0

#autres variables

base$femme <- 0
base$femme[base$sexe_r ==2] <-1

#Educ : <Bac = 1, Bac - bac +2 = 2, bac+3< = ref

base$educ_1 <- 0
base$educ_2 <- 0
base$educ_1[base$qs26_r == "01"|base$qs26_r == "02"|base$qs26_r == "03"|base$qs26_r == "04"|base$qs26_r == "05"|base$qs26_r == "06"|base$qs26_r == "07"|base$qs26_r == "08"] <- 1
base$educ_2[base$qs26_r == "09"|base$qs26_r == "10"|base$qs26_r == "11"] <- 1

#Secteur : energie = E, construction = c, vente = v, transport = t, manufacture = m, service = ref

base$secteur_e <- 0
base$secteur_c <- 0
base$secteur_v <- 0
base$secteur_t <- 0
base$secteur_m <- 0
base$secteur_e[base$nes16_r == "EG"] <- 1
base$secteur_c[base$nes16_r == "EH"] <- 1
base$secteur_v[base$nes16_r == "EJ"] <- 1
base$secteur_t[base$nes16_r == "EK"] <- 1
base$secteur_m[base$nes16_r == "EB" |base$nes16_r == "EC" |base$nes16_r == "ED" |base$nes16_r == "EE" |base$nes16_r == "EF"] <- 1

#Taille : <50 = 1, 50-99 : 2, 100-249 = 3, 250 - 499 = 4, >500 = ref

base$taille_1 <- 0
base$taille_2  <- 0
base$taille_3  <- 0
base$taille_4  <- 0
base$taille_1[base$tefen == 11|base$tefen == 12 ] <- 1
base$taille_2[base$tefen == 21] <- 1
base$taille_3[base$tefen == 22|base$tefen == 31] <- 1
base$taille_4[base$tefen == 32] <- 1

# IDF

base$idf <- 0
base$idf[base$dept == 77|base$dept == 78|base$dept == 92|base$dept == 93|base$dept == 94|base$dept == 95] <- 1


#Liste variables de contr??le : 
# base$femme base$educ_1 base$educ_2 base$secteur_e base$secteur_c base$secteur_v base$secteur_t base$secteur_m base$taille_1 base$taille_2 base$taille_3 base$taille_4 base$idf

##########################################################################################
# PSM avec interessement
##########################################################################################

# R??gression na??ve avec var de contr??le

model <- lm(base$sal_base ~ base$interessement +base$femme+ base$educ_1+ base$educ_2+ base$secteur_e+ base$secteur_c+ base$secteur_v+ base$secteur_t+ base$secteur_m+ base$taille_1+ base$taille_2+ base$taille_3+ base$taille_4+ base$idf)
summary(model)


stargazer(lm_final,type="html",
          dep.var.labels=c("Salaire de base"),
          covariate.labels=c("Interessement"),
          out = "lmfinint.htm")


# R??gression avec propensity score matching
# On va effectuer deux mod??les diff??rents : avec int??ressements et ??pargne salariale
# Avoir interessement = 1 ou ??pargne salariale = 1 ??quivalent ?? "trait??"
# Tuto suivi : https://sejdemyr.github.io/r-tutorials/statistics/tutorial8.html

# base$femme base$manager base$age_25 base$age_26_35 base$age_36_45 base$educ_1 base$educ_2 base$secteur_e base$secteur_c base$secteur_v base$secteur_t base$secteur_m base$taille_1 base$taille_2 base$taille_3 base$taille_4 base$paris base$idf base$comit??

library(MatchIt)
library(dplyr)
library(ggplot2)

# Analyse simple par interessement ou non

base %>%
  group_by(interessement) %>%
  summarise(n_salaries = n(),
            mean_salaire = mean(sal_base),
            std_error = sd(base$sal_base) / sqrt(n_salaries))

# Test d'une r??elle diff??rence de sal_base

t.test(base$sal_base ~ base$interessement)

#On va utiliser des covariables simples : secteur d'??tablissements, tailles de l'??tablissement,paris, idf, comit?? d'entreprise

base_cov <- c('femme','educ_1', 'educ_2', 'secteur_e','secteur_c','secteur_v','secteur_t','secteur_m','taille_1','taille_2','taille_3','taille_4','idf')
base %>%
  group_by(interessement) %>%
  select(one_of(base_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

# On voit une bonne diff??rence et apr??s test ces covariables sontcr??dibles pour g??n??rer interessement

# Entrons dans le vif du sujet : on estime d'abord un probit
# Il est n??cessaire de choisir des covariables qui sont en lien avec le salaire mais surtout la pr??sence d'int??ressement : c'est le cas de celles au dessus

m_ps <- glm(interessement ~ base$femme+ base$educ_1+ base$educ_2+ base$idf + base$secteur_e + base$secteur_c + base$secteur_v + base$secteur_t + base$secteur_m + base$taille_1 + base$taille_2 + base$taille_3 + base$taille_4,
            family = binomial(), data = base)
summary(m_ps)

stargazer(m_ps, type="html",
          dep.var.labels =c("Interessement"),
          covariate.labels =c("Sexe","Bac et inferieur",">Bac","Ile de France","Secteur de l'energie","Secteur de la constrution","Secteur du commerce","Secteur des transports","Secteur de l'industrie","10 ?? 49 salaries","50 ?? 99 salaries","100 ?? 249 salaries","250 ?? 499 salaries"),
          out="mps.doc")

# On effectue maintenant le propensity score : la probabilit?? pr??dite d'un salari?? d'??tre "trait??" ie faire partie d'une entreprise proposant de l'int??ressement

prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     interessement = m_ps$model$interessement)
head(prs_df)

#Plot de tout ??a 

labs <- paste("Interessement propos??", c("Oui", "Non"))
prs_df %>%
  mutate(interessement = ifelse(interessement == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~interessement) +
  xlab("Probabilit?? d'avoir un interessement propos??") +
  theme_bw()


#Algorithme de matching

base_nomiss <- base %>%  # le package match it n'autorise pas de NA : dans le doute on tej tout
  select(sal_base, interessement, one_of(base_cov)) %>%
  na.omit()

mod_match <- matchit(interessement ~ base$femme+ base$educ_1+ base$educ_2+ base$idf + base$secteur_e + base$secteur_c + base$secteur_v + base$secteur_t + base$secteur_m + base$taille_1 + base$taille_2 + base$taille_3 + base$taille_4,
                     method = "nearest", data = base_nomiss)

# On stock les donn??es match??es dans dta_m

dta_m <- match.data(mod_match)
dim(dta_m)


# On recompare les ??carts entre groupe trait?? et non trait??

dta_m %>%
  group_by(interessement) %>%
  select(one_of(base_cov)) %>%
  summarise_all(funs(mean))

# On peut maintenant estimer la diff??rence ! 

lm_final <- lm(sal_base ~ interessement, data = dta_m)
summary(lm_final)

stargazer(lm_final,type="html",
          dep.var.labels=c("Salaire de base"),
          covariate.labels=c("Interessement"),
          out = "lmfinint.htm")

# Comparaison de moyenne match??/non match??

dta_m %>%
  group_by(interessement) %>%
  summarise(n_salaries = n(),
            mean_salaire = mean(sal_base),
            std_error = sd(base$sal_base) / sqrt(n_salaries))


base %>%
  group_by(interessement) %>%
  summarise(n_salaries = n(),
            mean_salaire = mean(sal_base),
            std_error = sd(base$sal_base) / sqrt(n_salaries))

############################################################################################
# PSM avec plan ??pargne
############################################################################################


# R??gression avec propensity score matching
# On va effectuer deux mod??les diff??rents : avec int??ressements et ??pargne salariale
# Avoir interessement = 1 ou ??pargne salariale = 1 ??quivalent ?? "trait??"
# Tutot suivi : https://sejdemyr.github.io/r-tutorials/statistics/tutorial8.html

# base$femme base$educ_1 base$educ_2 base$secteur_e base$secteur_c base$secteur_v base$secteur_t base$secteur_m 
# base$taille_1 base$taille_2 base$taille_3 base$taille_4 base$idf



library(MatchIt)
library(dplyr)
library(ggplot2)

# Analyse simple par pee ou non

base %>%
  group_by(plan_epargne) %>%
  summarise(n_salaries = n(),
            mean_salaire = mean(sal_base),
            std_error = sd(base$sal_base) / sqrt(n_salaries))

# Test d'une r??elle diff??rence de sal_base

with(base, t.test(sal_base ~ plan_epargne)) #Valid?? ?? 1% de tranquilit?? fr??re

#On va utiliser des covariables simples : femme, educ_1, educ_2, secteur d'??tablissement, taille de l'??tablissement, idf

base_cov <- c('femme','educ_1', 'educ_2', 'secteur_e','secteur_c','secteur_v','secteur_t','secteur_m','taille_1','taille_2','taille_3','taille_4','idf')
base %>%
  group_by(plan_epargne) %>%
  select(one_of(base_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

# On voit une bonne diff??rence et apr??s test ces covariables sont cr??dibles pour g??n??rer interessement

# Entrons dans le vif du sujet : on estime d'abord un probit
# Il est n??cessaire de choisir des covariables qui sont en lien avec le salaire mais surtout la pr??sence d'int??ressement : c'est le cas de celles au dessus

m_ps2 <- glm(plan_epargne ~ base$femme+ base$educ_1+ base$educ_2+ base$idf + base$secteur_e + base$secteur_c + base$secteur_v + base$secteur_t + base$secteur_m + base$taille_1 + base$taille_2 + base$taille_3 + base$taille_4,
            family = binomial(), data = base)
summary(m_ps2)

stargazer(m_ps2, type="html",
          dep.var.labels =c("Plan Epargne Entreprise"),
          covariate.labels =c("Sexe","Bac et inferieur",">Bac","Ile de France","Secteur de l'energie","Secteur de la constrution","Secteur du commerce","Secteur des transports","Secteur de l'industrie","10 ?? 49 salaries","50 ?? 99 salaries","100 ?? 249 salaries","250 ?? 499 salaries"),
          out="mps2.doc")

# On effectue maintenant le propensity score : la probabilit?? pr??dite d'un salari?? d'??tre "trait??" ie faire partie d'une entreprise proposant de l'int??ressement

prs_df2 <- data.frame(pr_score = predict(m_ps2, type = "response"),
                     plan_epargne = m_ps2$model$plan_epargne)
head(prs_df2)

#Plot 

labs <- paste("Plan ??pargne propos??", c("Oui", "Non"))
prs_df2 %>%
  mutate(plan_epargne = ifelse(plan_epargne == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~plan_epargne) +
  xlab("Probabilit?? d'avoir un plan ??pargne propos??") +
  theme_bw()

#Algorithme de matching

base_nomiss2 <- base %>%  # le package match it n'autorise pas de NA : dans le doute on tej tout
  select(sal_base, plan_epargne, one_of(base_cov)) %>%
  na.omit()

mod_match2 <- matchit(plan_epargne ~ base$femme+ base$educ_1+ base$educ_2+ base$idf + base$secteur_e + base$secteur_c + base$secteur_v + base$secteur_t + base$secteur_m + base$taille_1 + base$taille_2 + base$taille_3 + base$taille_4,
                     method = "nearest", data = base_nomiss2)

# On stock les donn??es match??es dans dta_m

dta_m2 <- match.data(mod_match2)
dim(dta_m2)


# On recompare les ??carts entre groupe trait?? et non trait??

dta_m2 %>%
  group_by(plan_epargne) %>%
  select(one_of(base_cov)) %>%
  summarise_all(funs(mean))

# On peut maintenant estimer la diff??rence ! 

lm_final2 <- lm(sal_base ~ plan_epargne, data = dta_m2)
summary(lm_final2)

stargazer(lm_final2,type="html",
          dep.var.labels=c("Salaire de base"),
          covariate.labels=c("Plan epargne entreprise"),
          out = "lmfinpee.htm")

# Comparaison de moyenne match??/non match??

dta_m2 %>%
  group_by(plan_epargne) %>%
  summarise(n_salaries = n(),
            mean_salaire = mean(sal_base),
            std_error = sd(base$sal_base) / sqrt(n_salaries))

base %>%
  group_by(plan_epargne) %>%
  summarise(n_salaries = n(),
            mean_salaire = mean(sal_base),
            std_error = sd(base$sal_base) / sqrt(n_salaries))

############################################################################################
# PSM sur interessement ET plan ??pargne
############################################################################################

base$ipe <- 0
base$ipe[base$interessement ==1 & base$plan_epargne ==1] <- 1


# R??gression avec propensity score matching
# On va effectuer deux mod??les diff??rents : avec int??ressements et ??pargne salariale
# Avoir interessement = 1 ou ??pargne salariale = 1 ??quivalent ?? "trait??"
# Tutot suivi : https://sejdemyr.github.io/r-tutorials/statistics/tutorial8.html

# base$femme base$manager base$age_25 base$age_26_35 base$age_36_45 base$educ_1 base$educ_2 base$secteur_e base$secteur_c base$secteur_v base$secteur_t base$secteur_m base$taille_1 base$taille_2 base$taille_3 base$taille_4 base$paris base$idf base$comit??



library(MatchIt)
library(dplyr)
library(ggplot2)

# Analyse simple par interessement/pee ou non

base %>%
  group_by(base$ipe) %>%
  summarise(n_salaries = n(),
            mean_salaire = mean(sal_base),
            std_error = sd(base$sal_base) / sqrt(n_salaries))

# Test d'une r??elle diff??rence de sal_base

with(base, t.test(sal_base ~ base$ipe))

#On va utiliser des covariables simples : secteur d'??tablissements, tailles de l'??tablissement,paris, idf, comit?? d'entreprise

base_cov <- c('femme', 'age_25','educ_1', 'educ_2', 'secteur_e','secteur_c','secteur_v','secteur_t','secteur_m','taille_1','taille_2','taille_3','taille_4')
base %>%
  group_by(base$ipe) %>%
  select(one_of(base_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))




# On voit une bonne diff??rence et apr??s test ces covariables sontcr??dibles pour g??n??rer interessement

# Entrons dans le vif du sujet : on estime d'abord un probit
# Il est n??cessaire de choisir des covariables qui sont en lien avec le salaire mais surtout la pr??sence d'int??ressement : c'est le cas de celles au dessus

m_ps3 <- glm(ipe ~ base$femme+ base$educ_1+ educ_2+ base$idf + base$secteur_e + base$secteur_c + base$secteur_v + base$secteur_t + base$secteur_m + base$taille_1 + base$taille_2 + base$taille_3 + base$taille_4,
             family = binomial(), data = base)
summary(m_ps3)

stargazer(m_ps3, type="html",
          dep.var.labels =c("Interssement ou Plan Epargne Entreprise"),
          covariate.labels =c("Sexe","Bac et inferieur",">Bac","Ile de France","Secteur de l'energie","Secteur de la constrution","Secteur du commerce","Secteur des transports","Secteur de l'industrie","10 ?? 49 salaries","50 ?? 99 salaries","100 ?? 249 salaries","250 ?? 499 salaries"),
          out="mps3.doc")

# On effectue maintenant le propensity score : la probabilit?? pr??dite d'un salari?? d'??tre "trait??" ie faire partie d'une entreprise proposant de l'int??ressement ou un pee

prs_df3 <- data.frame(pr_score = predict(m_ps3, type = "response"),
                      ipe = m_ps3$model$ipe)
head(prs_df3)


#Plot du propensity score 

labs <- paste("Int??ressement et plan ??pargne propos??", c("Oui", "Non"))
prs_df %>%
  mutate(ipe = ifelse(base$ipe == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~ipe) +
  xlab("Probabilit?? d'avoir les deux propos??s") +
  theme_bw()


#Algorithme de matching

base_nomiss <- base %>%  # le package match it n'autorise pas de NA : dans le doute on tej tout
  select(sal_base, ipe, one_of(base_cov)) %>%
  na.omit()

mod_match <- matchit(ipe ~ base$femme+ base$educ_1+ educ_2+ base$idf + base$secteur_e + base$secteur_c + base$secteur_v + base$secteur_t + base$secteur_m + base$taille_1 + base$taille_2 + base$taille_3 + base$taille_4,
                     method = "nearest", data = base_nomiss)

# On stock les donn??es match??es dans dta_m

dta_m <- match.data(mod_match)
dim(dta_m)


# On recompare les ??carts entre groupe trait?? et non trait??

dta_m %>%
  group_by(base$ipe) %>%
  select(one_of(base_cov)) %>%
  summarise_all(funs(mean))

# On peut maintenant estimer la diff??rence ! 

lm_final3 <- lm(sal_base ~ ipe, data = dta_m)
summary(lm_final3)


stargazer(lm_final3,type="html",
          dep.var.labels=c("Salaire de base"),
          covariate.labels=c("Interessement et Plan epargne entreprise"),
          out = "lmfinipe.htm")

# Comparaison de moyenne match??/non match??

dta_m %>%
  group_by(ipe) %>%
  summarise(n_salaries = n(),
            mean_salaire = mean(sal_base),
            std_error = sd(base$sal_base) / sqrt(n_salaries))

base %>%
  group_by(base$ipe) %>%
  summarise(n_salaries = n(),
            mean_salaire = mean(sal_base),
            std_error = sd(base$sal_base) / sqrt(n_salaries))
