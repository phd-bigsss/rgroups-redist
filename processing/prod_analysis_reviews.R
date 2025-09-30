set1 <-  RColorBrewer::brewer.pal(n = 8, name = "Set1")
options(ggplot2.discrete.fill = set1)
options(ggplot2.discrete.colour = set1)
ggplot2::theme_set(ggplot2::theme_bw())
ggplot2::theme_update(text=ggplot2::element_text(size=15,  family="serif"))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,sjmisc,ggplot2,interplot,marginaleffects,sjlabelled,haven,stringr,ordinal,texreg,MLMusingR,lme4)  
rm(list=ls())

load(here::here("input/data/proc/study1_country.RData"));df1 <- df2
dfreg <- df1 %>% dplyr::select(
  prefineq,redist,
  egal = egal2,
  dclass3res_V,
  dclass6res,
  homclass3_V_res,
  know_total,
  n_low,
  n_middle,
  n_high,
  Q03pcm,
  edyears,
  female,
  agenum,
  partner,
  workst, 
  WEIGHT,
  region,
  "gini_disp"=wid_gini_disp,
  "gini_mkt",
  gv_spen,
  rel_red,
  d10d1=wid_p90p10,
  wid_p90p50,
  top10=wid_sharetop10,
  rgdpna,
  gdppercapita,
  oecd,
  country2, country, country3,
) %>%
  mutate(logrgdpna = log(rgdpna),
         loggdppercapita=log(gdppercapita),
         edyears2 = edyears ^ 2,
         age2 = agenum ^ 2) %>%
  filter(country2 != "SVN") 


dfreg <- na.omit(dfreg)



dfreg$n_low <- (dfreg$n_low - min(dfreg$n_low)) / (max(dfreg$n_low) - min(dfreg$n_low))*100
dfreg$n_middle <- (dfreg$n_middle - min(dfreg$n_middle)) / (max(dfreg$n_middle) - min(dfreg$n_middle))*100
dfreg$n_high <- (dfreg$n_high - min(dfreg$n_high)) / (max(dfreg$n_high) - min(dfreg$n_high))*100


# Single class-profile analysis -------------------------------------------
digclas1_VW_0<- lmer(egal~1 + (1|country2),data=dfreg)
digclas1_VW_1<- lmer(egal~1 + n_low+n_middle+n_high+(1|country2),data=dfreg)
digclas1_VW_2<- lmer(egal~1 + female+agenum+partner+edyears+Q03pcm+workst+(1|country2),data=dfreg)
digclas1_VW_3 <- update(digclas1_VW_2, . ~ . + dclass3res_V)
digclas1_VW_4 <- update(digclas1_VW_2, . ~ . + dclass3res_V+homclass3_V_res)
digclas1_VW_8 <- update(digclas1_VW_3, . ~ . +dclass3res_V*n_high+dclass3res_V*n_middle+dclass3res_V*n_low+homclass3_V_res)

# interaction of homogeneity with individual class position while controlling by the number of network ties (this is also contained in the homogeneity measure)
digclas1_VW_9 <- update(digclas1_VW_3, . ~ . +dclass3res_V*homclass3_V_res+n_low+n_middle+n_high)
# interaction of number of network ties with individual class and network homogeneity with individual class
digclas1_VW_9 <- update(digclas1_VW_3, . ~ . +dclass3res_V*n_high+dclass3res_V*n_middle+dclass3res_V*n_low+homclass3_V_res+dclass3res_V*homclass3_V_res) 

digclas1_VW <- list(digclas1_VW_0,digclas1_VW_1,digclas1_VW_2,
                    digclas1_VW_3,digclas1_VW_4,
                    digclas1_VW_8,
                    digclas1_VW_9
                    )
screenreg(digclas1_VW)
# 6 class scheme  ---------------------------------------------------------
## Model -  EGP6 - DIGCLASS - V to Working -----------------------------$ 
digclas1_VW_0<- lmer(egal~1 + homclass3_V_res + (1|country2),data=dfreg,weights = WEIGHT)
digclas1_VW_1<- lmer(egal~1 + homclass3_V_res+know_total +female+agenum+partner+edyears+Q03pcm+workst+(1|country2),data=dfreg,weights = WEIGHT)
digclas1_VW_3 <- update(digclas1_VW_1, . ~ . + dclass6res)

digclas1_VW_7 <- update(digclas1_VW_3, . ~ . +dclass6res*homclass3_V_res)
digclas1_VW <- list(digclas1_VW_0,digclas1_VW_1,

                    digclas1_VW_7)

screenreg(digclas1_VW)

# Cross-level interactions ------------------------------------------------
set1 <-  RColorBrewer::brewer.pal(n = 8, name = "Set1")
options(ggplot2.discrete.fill = set1)
options(ggplot2.discrete.colour = set1)
ggplot2::theme_set(ggplot2::theme_bw())
ggplot2::theme_update(text=ggplot2::element_text(size=15,  family="serif"))
options(scipen=999)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,sjmisc,ggplot2,interplot,marginaleffects,sjlabelled,haven,stringr,ordinal,texreg,MLMusingR,lme4)  
rm(list=ls()) 

load(here::here("input/data/proc/study1_country.RData"));df1 <- df2

df1$dclass3spo_V <- car::recode(df1$dclass3spo_V,"NA='No information (Missing, No partner)'",levels = c("Service Class (I+II)","Intermediate class (III+IV)","Working Class (V+VI+VII)","No information (Missing, No partner)"))

dfreg <- df1 %>% dplyr::select(
  redist,prefineq,
  egal = egal2,
  dclass3res_V,
  homclass3_V_res,
  know_total,
  Q03pcm,
  edyears,
  female,
  agenum,
  partner,
  workst,
  WEIGHT,
  region,
  "gini_disp"=difclass,
  "gini_mkt",
  gv_spen,
  rel_red,
  abs_red,
  ilo_taxrev,
  ilo_govspe,
  d10d1=wid_p90p10,
  wid_p90p50,
  top10=wid_sharetop10,
  rgdpna,
  gdppercapita,
  oecd,
  country2, country, country3,
) %>%
  filter(country2 != "SVN") %>% 
  na.omit() %>% 
  mutate(loggdppercapita=scale(gdppercapita/1000),
         edyears2 = edyears ^ 2,
         age2 = agenum ^ 2, 
         gini_disp=scale(gini_disp))

dfreg$wstate2 <- scale(scale(dfreg$abs_red) + scale(dfreg$ilo_taxrev) + scale(dfreg$ilo_govspe))

# Variable Group-Centering ------------------------------------------------$
dfreg <- 
  dfreg %>% 
  mutate(to_dummy(female),
         to_dummy(workst),
         # dummy for categorical variables-------------------------------------$
         to_dummy(Q03pcm),
  )
dfreg$female_gc = group_center(dfreg$female_2, grp = dfreg$country2)
dfreg$workst_gc = group_center(dfreg$workst_2, grp = dfreg$country2)
dfreg$edyears_gc = group_center(dfreg$edyears, grp = dfreg$country2)
dfreg$agenum_gc = group_center(dfreg$agenum, grp = dfreg$country2)
dfreg$age2_gc = group_center(dfreg$age2, grp = dfreg$country2)
dfreg$homclass3_V_gc <-group_center(dfreg$homclass3_V_res, grp = dfreg$country2)
dfreg$know_total_gc = group_center(dfreg$know_total, grp = dfreg$country2)
dfreg$partner_gc = group_center(as.numeric(dfreg$partner), grp = dfreg$country2)

# Household Income Tercile
dfreg$Q03pcm_1_gc = group_center(dfreg$Q03pcm_1, grp = dfreg$country2)
dfreg$Q03pcm_2_gc = group_center(dfreg$Q03pcm_2, grp = dfreg$country2)
dfreg$Q03pcm_3_gc = group_center(dfreg$Q03pcm_3, grp = dfreg$country2)
dfreg$Q03pcm_NA_gc = group_center(dfreg$Q03pcm_4, grp = dfreg$country2)

# drop dummies
dfreg <- dplyr::select(dfreg,-c(female_1,female_2,
                                # union_1,union_2,workst_1,
                                workst_2,Q03pcm_1,Q03pcm_2,Q03pcm_2,Q03pcm_4,
))


homo_V_gini_gdp_controls <-
  lmer(egal~
         homclass3_V_gc+
         know_total_gc+
         dclass3res_V+
         female_gc+agenum_gc+
         partner_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         workst_gc+
         gini_disp +loggdppercapita + wstate2 +
         (homclass3_V_gc+dclass3res_V|country2),data=dfreg,weights = WEIGHT)

homo_V_gini_full2 <- 
  lmer(egal~ 
         homclass3_V_gc+
         know_total_gc+
         dclass3res_V+
         female_gc+agenum_gc+
         partner_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         workst_gc+
         gini_disp +loggdppercapita + wstate2+
         homclass3_V_gc*
         dclass3res_V*
         gini_disp + 
         (homclass3_V_gc+dclass3res_V|country2),data=dfreg,weights = WEIGHT)

homo_V_wstate_full2 <- 
  lmer(egal~ 
         homclass3_V_gc+
         know_total_gc+
         dclass3res_V+
         female_gc+agenum_gc+
         partner_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         workst_gc+
         gini_disp +loggdppercapita + wstate2+
         homclass3_V_gc*
         dclass3res_V*
         wstate2 + 
         (homclass3_V_gc+dclass3res_V|country2),data=dfreg,weights = WEIGHT)

homo_V_wstate_gini <- 
  lmer(egal~ 
         homclass3_V_gc+
         know_total_gc+
         dclass3res_V+
         female_gc+agenum_gc+
         partner_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         workst_gc+
         gini_disp*wstate2 +loggdppercapita + wstate2+
         (homclass3_V_gc+dclass3res_V|country2),data=dfreg,weights = WEIGHT)


homo_V_gini_wstate_full <- 
  lmer(egal~ 
         homclass3_V_gc+
         know_total_gc+
         dclass3res_V+
         female_gc+agenum_gc+
         partner_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         workst_gc+
         gini_disp*wstate2 +loggdppercapita + wstate2+
         homclass3_V_gc*
         dclass3res_V*
         gini_disp + 
         (homclass3_V_gc+dclass3res_V|country2),data=dfreg,weights = WEIGHT)


homo_V_gini_wstate_full2 <- 
  lmer(egal~ 
         homclass3_V_gc+
         know_total_gc+
         dclass3res_V+
         female_gc+agenum_gc+
         partner_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         workst_gc+
         gini_disp +loggdppercapita + wstate2+
         homclass3_V_gc*
         dclass3res_V*
         gini_disp + 
         homclass3_V_gc*
         dclass3res_V*
         wstate2 + 
         (homclass3_V_gc+dclass3res_V|country2),data=dfreg,weights = WEIGHT)


digclas1_macro <- list(homo_V_gini_full2,homo_V_wstate_full2,homo_V_wstate_gini,homo_V_gini_wstate_full,homo_V_gini_wstate_full2)
screenreg(digclas1_macro)

