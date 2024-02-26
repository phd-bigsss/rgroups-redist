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
  egal = egal2,
  starts_with("class3"),
  starts_with("digclass3"),
  starts_with("dclass3"),
  starts_with("homclass"),
  know_total,
  socialtrust,
  Q03pcm,
  edyears,
  female,
  agenum,
  union,
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
  country2, country
) %>%
  mutate(logrgdpna = log(rgdpna),
         loggdppercapita=log(gdppercapita),
         # know_total=log(know_total),
         edyears2 = edyears ^ 2,
         age2 = agenum ^ 2) %>%
  # filter(oecd == "OECD") %>%
  filter(country2 != "SVN")  
  # filter(country2 != "ZAF") %>%
  # filter(country2 != "HUN") 

dfreg <- 
  dfreg %>% 
  mutate(to_dummy(female),
         to_dummy(union),
         to_dummy(workst),
         # dummy for categorical variables-------------------------------------
         to_dummy(class3),
         # to_dummy(class3spo),
         # to_dummy(class3res),
         to_dummy(Q03pcm)
  )
dfreg$female_gc = group_center(dfreg$female_2, grp = dfreg$country2)
dfreg$union_gc = group_center(dfreg$union_2, grp = dfreg$country2)
dfreg$workst_gc = group_center(dfreg$workst_2, grp = dfreg$country2)
dfreg$edyears_gc = group_center(dfreg$edyears, grp = dfreg$country2)
dfreg$agenum_gc = group_center(dfreg$agenum, grp = dfreg$country2)
dfreg$age2_gc = group_center(dfreg$age2, grp = dfreg$country2)
dfreg$socialtrust_gc = group_center(dfreg$socialtrust, grp = dfreg$country2)


dfreg$homclass_gc  <-
  group_center(dfreg$homclass_res, grp = dfreg$country2)
dfreg$homclass_V_gc <-
  group_center(dfreg$homclass_V_res, grp = dfreg$country2)
dfreg$homclass2_gc <-
  group_center(dfreg$homclass2_res, grp = dfreg$country2)
dfreg$homclass2_V_gc <-
  group_center(dfreg$homclass2_V_res, grp = dfreg$country2)
dfreg$homclass3_gc <-
  group_center(dfreg$homclass3_res, grp = dfreg$country2)
dfreg$homclass3_V_gc <-
  group_center(dfreg$homclass3_V_res, grp = dfreg$country2)
dfreg$homclass3_III_gc <-
  group_center(dfreg$homclass3_III_res, grp = dfreg$country2)
dfreg$homclass3_III_V_gc <-
  group_center(dfreg$homclass3_III_V_res, grp = dfreg$country2)




dfreg$know_total_gc = group_center(dfreg$know_total, grp = dfreg$country2)

# EGP: dominance
dfreg$class3_ser_gc = group_center(dfreg$class3_1, grp = dfreg$country2)
dfreg$class3_mid_gc = group_center(dfreg$class3_2, grp = dfreg$country2)
dfreg$class3_wor_gc = group_center(dfreg$class3_3, grp = dfreg$country2)
# EGP: Respondent
# dfreg$class3res_ser_gc = group_center(dfreg$class3res_1, grp = dfreg$country2)
# dfreg$class3res_mid_gc = group_center(dfreg$class3res_2, grp = dfreg$country2)
# dfreg$class3res_wor_gc = group_center(dfreg$class3res_3, grp = dfreg$country2)
# EGP: Respondent
# dfreg$class3spo_ser_gc = group_center(dfreg$class3spo_1, grp = dfreg$country2)
# dfreg$class3spo_mid_gc = group_center(dfreg$class3spo_2, grp = dfreg$country2)
# dfreg$class3spo_wor_gc = group_center(dfreg$class3spo_3, grp = dfreg$country2)
# Household Income Tercile
dfreg$Q03pcm_1_gc = group_center(dfreg$Q03pcm_1, grp = dfreg$country2)
dfreg$Q03pcm_2_gc = group_center(dfreg$Q03pcm_2, grp = dfreg$country2)
dfreg$Q03pcm_3_gc = group_center(dfreg$Q03pcm_3, grp = dfreg$country2)
dfreg$Q03pcm_NA_gc = group_center(dfreg$Q03pcm_4, grp = dfreg$country2)

names(dfreg)
# drop dummies
dfreg <- 
  dfreg %>% 
  dplyr::select(-c(female_1,female_2,union_1,union_2,workst_1,workst_2,
                   class3_1,class3_2,class3_3,
                   # class3res_1,class3res_2,class3res_3,
                   # class3spo_1,class3spo_2,class3spo_3,
                   Q03pcm_1,Q03pcm_2,Q03pcm_2,Q03pcm_4))
names(dfreg)

dfreg$class3spo <- car::recode(dfreg$class3spo,"NA='Missing'")
dfreg$class3spo_V <- car::recode(dfreg$class3spo_V,"NA='Missing'")
dfreg$digclass3spo <- car::recode(dfreg$digclass3spo,"NA='Missing'")
dfreg$digclass3spo_V <- car::recode(dfreg$digclass3spo_V,"NA='Missing'")

dfreg$dclass3spo <- car::recode(dfreg$dclass3spo,"NA='Missing'")
dfreg$dclass3spo_V <- car::recode(dfreg$dclass3spo_V,"NA='Missing'")
dfreg$dclass3spo_III<- car::recode(dfreg$dclass3spo_III,"NA='Missing'")
dfreg$dclass3spo_III_V<- car::recode(dfreg$dclass3spo_III_V,"NA='Missing'")



# Model -  EGP3 - isko v1 - V to Intermediate -----------------------------
isko1_VI_1<- lmer(egal~1 + homclass_res +(1|country2),data=dfreg,weights = WEIGHT)
isko1_VI_2<- update(isko1_VI_1, . ~ . +know_total+socialtrust)
isko1_VI_3 <- update(isko1_VI_2, . ~ . + class3res+female+agenum+age2)
isko1_VI_4 <- update(isko1_VI_3, . ~ . + class3res+edyears+ Q03pcm+union+workst +class3spo)
isko1_VI_5 <- update(isko1_VI_4, . ~ . +class3res*homclass_res)
isko1_VI <- list(isko1_VI_1,isko1_VI_2,isko1_VI_3,isko1_VI_4,isko1_VI_5)
screenreg(isko1_VI,stars = c(0.001, 0.01, 0.05, 0.1),symbol = "+")

# Model -  EGP3 - isko v1 - V to Working -----------------------------------
isko1_VW_1<- lmer(egal~1 + homclass_V_res +(1|country2),data=dfreg,weights = WEIGHT)
isko1_VW_2<- update(isko1_VW_1, . ~ . +know_total+socialtrust)
isko1_VW_3 <- update(isko1_VW_2, . ~ . + class3res_V+female+agenum+age2)
isko1_VW_4 <- update(isko1_VW_3, . ~ . + class3res_V+edyears+ Q03pcm+union+workst+class3spo_V)
isko1_VW_5 <- update(isko1_VW_4, . ~ . +class3res_V*homclass_V_res)

isko1_VM <- list(isko1_VW_1,isko1_VW_2,isko1_VW_3,isko1_VW_4,isko1_VW_5)
screenreg(isko1_VM,stars = c(0.001, 0.01, 0.05, 0.1),symbol = "+")

# Model -  EGP3 - isko v2 - V to Intermediate -----------------------------
isko2_VI_1<- lmer(egal~1 + homclass2_res +(1|country2),data=dfreg,weights = WEIGHT)
isko2_VI_2<- update(isko2_VI_1, . ~ . +know_total+socialtrust)
isko2_VI_3 <- update(isko2_VI_2, . ~ . + digclass3res+female+agenum+age2)
isko2_VI_4 <- update(isko2_VI_3, . ~ . + digclass3res+edyears+ Q03pcm+union+workst+digclass3spo)
isko2_VI_5 <- update(isko2_VI_4, . ~ . +digclass3res*homclass2_res)
isko2_VI <- list(isko2_VI_1,isko2_VI_2,isko2_VI_3,isko2_VI_4,isko2_VI_5)
screenreg(isko2_VI,stars = c(0.001, 0.01, 0.05, 0.1),symbol = "+")

# Model -  EGP3 - isko v2 - V to Working -----------------------------
isko2_VW_1<- lmer(egal~1 + homclass2_V_res +(1|country2),data=dfreg,weights = WEIGHT)
isko2_VW_2<- update(isko2_VW_1, . ~ . +know_total+socialtrust)
isko2_VW_3 <- update(isko2_VW_2, . ~ . + digclass3res_V+female+agenum+age2)
isko2_VW_4 <- update(isko2_VW_3, . ~ . + digclass3res_V+edyears+ Q03pcm+union+workst+digclass3spo_V)
isko2_VW_5 <- update(isko2_VW_4, . ~ . +digclass3res_V*homclass2_V_res)
isko2_VW <- list(isko2_VW_1,isko2_VW_2,isko2_VW_3,isko2_VW_4,isko2_VW_5)
screenreg(isko2_VW,stars = c(0.001, 0.01, 0.05, 0.1),symbol = "+")


# Model -  EGP3 - DIGCLASS - V to Intermediate -----------------------------
digclas1_VI_1<- lmer(egal~1 + homclass3_res +(1|country2),data=dfreg,weights = WEIGHT)
digclas1_VI_2<- update(digclas1_VI_1, . ~ . +know_total+socialtrust)
digclas1_VI_3 <- update(digclas1_VI_2, . ~ . + dclass3res+female+agenum+age2)
digclas1_VI_4 <- update(digclas1_VI_3, . ~ . + dclass3res+edyears+ Q03pcm+union+workst+dclass3spo)
digclas1_VI_5 <- update(digclas1_VI_4, . ~ . +dclass3res*homclass3_res)
digclas1_VI <- list(digclas1_VI_1,digclas1_VI_2,digclas1_VI_3,digclas1_VI_4,digclas1_VI_5)
screenreg(digclas1_VI,stars = c(0.001, 0.01, 0.05, 0.1),symbol = "+")


# Model -  EGP3 - DIGCLASS - V to Working -----------------------------
digclas1_VW_1<- lmer(egal~1 + homclass3_V_res +(1|country2),data=dfreg,weights = WEIGHT)
digclas1_VW_2<- update(digclas1_VW_1, . ~ . +know_total+socialtrust)
digclas1_VW_3 <- update(digclas1_VW_2, . ~ . + dclass3res_V+female+agenum+age2)
digclas1_VW_4 <- update(digclas1_VW_3, . ~ . + dclass3res_V+edyears+ Q03pcm+union+workst+dclass3spo_V)
digclas1_VW_5 <- update(digclas1_VW_4, . ~ . +dclass3_V*homclass3_V_res)
digclas1_VW <- list(digclas1_VW_1,digclas1_VW_2,digclas1_VW_3,digclas1_VW_4,digclas1_VW_5)
screenreg(digclas1_VW,stars = c(0.001, 0.01, 0.05, 0.1),symbol = "+")


# Model -  EGP3 - DIGCLASS - IIIb to Working -----------------------------
digclas1_IIIW_1<- lmer(egal~1 + homclass3_III_res +(1|country2),data=dfreg,weights = WEIGHT)
digclas1_IIIW_2<- update(digclas1_IIIW_1, . ~ . +know_total+socialtrust)
digclas1_IIIW_3 <- update(digclas1_IIIW_2, . ~ . + dclass3res_III+female+agenum+age2)
digclas1_IIIW_4 <- update(digclas1_IIIW_3, . ~ . + dclass3res_III+edyears+ Q03pcm+union+workst+dclass3spo_III)
digclas1_IIIW_5 <- update(digclas1_IIIW_4, . ~ . +dclass3res_III*homclass3_III_res)
digclas1_IIIW <- list(digclas1_IIIW_1,digclas1_IIIW_2,digclas1_IIIW_3,digclas1_IIIW_4,digclas1_IIIW_5)
screenreg(digclas1_IIIW,stars = c(0.001, 0.01, 0.05, 0.1),symbol = "+")


# Model -  EGP3 - DIGCLASS - IIIb+V to Working -----------------------------
digclas1_III_V_W_1<- lmer(egal~1 + homclass3_III_V_res +(1|country2),data=dfreg,weights = WEIGHT)
digclas1_III_V_W_2<- update(digclas1_III_V_W_1, . ~ . +know_total+socialtrust)
digclas1_III_V_W_3 <- update(digclas1_III_V_W_2, . ~ . + dclass3res_III_V+female+agenum+age2)
digclas1_III_V_W_4 <- update(digclas1_III_V_W_3, . ~ . + dclass3res_III_V+edyears+ Q03pcm+union+workst+dclass3spo_III_V)
digclas1_III_V_W_5 <- update(digclas1_III_V_W_4, . ~ . +dclass3res_III_V*homclass3_III_V_res)
digclas1_III_V_W <- list(digclas1_III_V_W_1,digclas1_III_V_W_2,digclas1_III_V_W_3,digclas1_III_V_W_4,digclas1_III_V_W_5)
screenreg(digclas1_III_V_W,stars = c(0.001, 0.01, 0.05, 0.1),symbol = "+")


# Contrast between operationalizations ------------------------------------
screenreg(list(isko1_VI_5,isko1_VW_5,isko2_VI_5,isko2_VW_5,digclas1_VI_5,digclas1_VW_5,digclas1_IIIW_5,digclas1_III_V_W_5),
          stars = c(0.001, 0.01, 0.05, 0.1),
          # file="output/tables/micro_homo-class3_diff-operat-full-resp_spouse.txt",
          # "output/tables/micro_homo-class3_diff-operat-oecd-resp_spouse.txt",
          single.row = T)


# Macro level models ------------------------------------------------------
dfreg <- dfreg %>% mutate_all(~ifelse(is.nan(.), NA, .))
## Gini Disposable---------------------------------------------------------
homo_giniD <- 
  lmer(egal~ homclass_gc+know_total_gc+socialtrust_gc+class3res+class3spo+female_gc+agenum_gc+age2_gc+edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+union_gc+workst_gc+gini_disp +loggdppercapita + rel_red + gv_spen + homclass_gc*class3res*gini_disp +(homclass_gc+class3res|country2),data=dfreg,weights = WEIGHT)

homo_V_giniD <- 
  lmer(egal~ 
         homclass_V_gc+
         know_total_gc+socialtrust_gc+
         class3res_V+class3spo_V+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         gini_disp +loggdppercapita + rel_red + gv_spen +
         homclass_V_gc*
         class3res_V*
         gini_disp +
         (homclass_V_gc+class3res_V|country2),data=dfreg,weights = WEIGHT)

homo2_giniD <- 
  lmer(egal~ 
         homclass2_gc+
         know_total_gc+socialtrust_gc+
         digclass3res+digclass3spo+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         gini_disp +loggdppercapita + rel_red + gv_spen +
         homclass2_gc*
         digclass3res*
         gini_disp +
         (homclass2_gc+digclass3res|country2),data=dfreg,weights = WEIGHT)

homo2_V_giniD <- 
  lmer(egal~ 
         homclass2_V_gc+
         know_total_gc+socialtrust_gc+
         digclass3res_V+digclass3spo_V+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         gini_disp +loggdppercapita + rel_red + gv_spen +
         homclass2_V_gc*
         digclass3res_V*
         gini_disp +
         (homclass2_V_gc+digclass3res_V|country2),data=dfreg,weights = WEIGHT)

homo3_giniD <- 
  lmer(egal~ 
         homclass3_gc+
         know_total_gc+socialtrust_gc+
         dclass3res+dclass3spo+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         gini_disp +loggdppercapita + rel_red + gv_spen +
         homclass3_gc*
         dclass3res*
         gini_disp +
         (homclass3_gc+dclass3res|country2),data=dfreg,weights = WEIGHT)

homo3_V_giniD <- 
  lmer(egal~ 
         homclass3_V_gc+
         know_total_gc+socialtrust_gc+
         dclass3res_V+dclass3spo_V+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         gini_disp +loggdppercapita + rel_red + gv_spen +
         homclass3_V_gc*
         dclass3res_V*
         gini_disp +
         (homclass3_V_gc+dclass3res_V|country2),data=dfreg,weights = WEIGHT)

homo3_III_giniD <- 
  lmer(egal~ 
         homclass3_III_gc+
         know_total_gc+socialtrust_gc+
         dclass3res_III+dclass3spo_III+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         gini_disp +loggdppercapita + rel_red + gv_spen +
         homclass3_III_gc*
           dclass3res_III*
         gini_disp +
         (homclass3_III_gc+dclass3res_III|country2),data=dfreg,weights = WEIGHT)

homo3_III_V_giniD <- 
  lmer(egal~ 
         homclass3_III_V_gc+
         know_total_gc+socialtrust_gc+
         dclass3res_III_V+dclass3spo_III_V+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         gini_disp +loggdppercapita + rel_red + gv_spen +
         homclass3_III_V_gc*
         dclass3res_III_V*
         gini_disp +
         (homclass3_III_V_gc+dclass3res_III_V|country2),data=dfreg,weights = WEIGHT)


inte_giniD <-list(homo_giniD,homo2_giniD,homo2_V_giniD,homo3_giniD,homo3_V_giniD,homo3_III_giniD,homo3_III_V_giniD)

screenreg(inte_giniD,
          "output/tables/macro_homo-giniD_diff-operat-full-resp_spouse.txt",
          # "output/tables/macro_homo-giniD_diff-operat-oecd.txt",
          single.row = T)

## Ratio P90P10---------------------------------------------------------
homo_p90p10 <- 
  lmer(egal~ homclass_gc+know_total_gc+socialtrust_gc+class3+female_gc+agenum_gc+age2_gc+edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+union_gc+workst_gc+d10d1 +loggdppercapita + rel_red + gv_spen + homclass_gc*class3*d10d1 +(homclass_gc+class3|country2),data=dfreg,weights = WEIGHT)

homo_V_p90p10 <- 
  lmer(egal~ 
         homclass_V_gc+
         know_total_gc+socialtrust_gc+
         class3_V+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         d10d1 +loggdppercapita + rel_red + gv_spen +
         homclass_V_gc*
         class3_V*
         d10d1 +
         (homclass_V_gc+class3_V|country2),data=dfreg,weights = WEIGHT)

homo2_p90p10 <- 
  lmer(egal~ 
         homclass2_gc+
         know_total_gc+socialtrust_gc+
         digclass3+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         d10d1 +loggdppercapita + rel_red + gv_spen +
         homclass2_gc*
         digclass3*
         d10d1 +
         (homclass2_gc+digclass3|country2),data=dfreg,weights = WEIGHT)

homo2_V_p90p10 <- 
  lmer(egal~ 
         homclass2_V_gc+
         know_total_gc+socialtrust_gc+
         digclass3_V+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         d10d1 +loggdppercapita + rel_red + gv_spen +
         homclass2_V_gc*
         digclass3_V*
         d10d1 +
         (homclass2_V_gc+digclass3_V|country2),data=dfreg,weights = WEIGHT)

homo3_p90p10 <- 
  lmer(egal~ 
         homclass3_gc+
         know_total_gc+socialtrust_gc+
         dclass3+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         d10d1 +loggdppercapita + rel_red + gv_spen +
         homclass3_gc*
         dclass3*
         d10d1 +
         (homclass3_gc+dclass3|country2),data=dfreg,weights = WEIGHT)

homo3_V_p90p10 <- 
  lmer(egal~ 
         homclass3_V_gc+
         know_total_gc+socialtrust_gc+
         dclass3_V+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         d10d1 +loggdppercapita + rel_red + gv_spen +
         homclass3_V_gc*
         dclass3_V*
         d10d1 +
         (homclass3_V_gc+dclass3_V|country2),data=dfreg,weights = WEIGHT)

homo3_III_p90p10 <- 
  lmer(egal~ 
         homclass3_III_gc+
         know_total_gc+socialtrust_gc+
         dclass3_III+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         d10d1 +loggdppercapita + rel_red + gv_spen +
         homclass3_III_gc*
         dclass3_III*
         d10d1 +
         (homclass3_III_gc+dclass3_III|country2),data=dfreg,weights = WEIGHT)

homo3_III_V_p90p10 <- 
  lmer(egal~ 
         homclass3_III_V_gc+
         know_total_gc+socialtrust_gc+
         dclass3_III_V+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         d10d1 +loggdppercapita + rel_red + gv_spen +
         homclass3_III_V_gc*
         dclass3_III_V*
         d10d1 +
         (homclass3_III_V_gc+dclass3_III_V|country2),data=dfreg,weights = WEIGHT)


inte_p90p10 <-list(homo_p90p10,homo2_p90p10,homo2_V_p90p10,homo3_p90p10,homo3_V_p90p10,homo3_III_p90p10,homo3_III_V_p90p10)

screenreg(inte_p90p10,
          # "output/tables/macro_homo-p90p10_diff-operat-full.txt",
          # "output/tables/macro_homo-p90p10_diff-operat-oecd.txt",
          single.row = T)

## Ratio P90/BOT50---------------------------------------------------------
homo_wid_p90p50 <- 
  lmer(egal~ homclass_gc+know_total_gc+socialtrust_gc+class3+female_gc+agenum_gc+age2_gc+edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+union_gc+workst_gc+wid_p90p50 +loggdppercapita + rel_red + gv_spen + homclass_gc*class3*d10d1 +(homclass_gc+class3|country2),data=dfreg,weights = WEIGHT)

homo_V_wid_p90p50 <- 
  lmer(egal~ 
         homclass_V_gc+
         know_total_gc+socialtrust_gc+
         class3_V+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         wid_p90p50 +loggdppercapita + rel_red + gv_spen +
         homclass_V_gc*
         class3_V*
         wid_p90p50 +
         (homclass_V_gc+class3_V|country2),data=dfreg,weights = WEIGHT)

homo2_wid_p90p50 <- 
  lmer(egal~ 
         homclass2_gc+
         know_total_gc+socialtrust_gc+
         digclass3+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         wid_p90p50 +loggdppercapita + rel_red + gv_spen +
         homclass2_gc*
         digclass3*
         wid_p90p50 +
         (homclass2_gc+digclass3|country2),data=dfreg,weights = WEIGHT)

homo2_V_wid_p90p50 <- 
  lmer(egal~ 
         homclass2_V_gc+
         know_total_gc+socialtrust_gc+
         digclass3_V+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         wid_p90p50 +loggdppercapita + rel_red + gv_spen +
         homclass2_V_gc*
         digclass3_V*
         wid_p90p50 +
         (homclass2_V_gc+digclass3_V|country2),data=dfreg,weights = WEIGHT)

homo3_wid_p90p50 <- 
  lmer(egal~ 
         homclass3_gc+
         know_total_gc+socialtrust_gc+
         dclass3+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         wid_p90p50 +loggdppercapita + rel_red + gv_spen +
         homclass3_gc*
         dclass3*
         wid_p90p50 +
         (homclass3_gc+dclass3|country2),data=dfreg,weights = WEIGHT)

homo3_V_wid_p90p50 <- 
  lmer(egal~ 
         homclass3_V_gc+
         know_total_gc+socialtrust_gc+
         dclass3_V+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         wid_p90p50 +loggdppercapita + rel_red + gv_spen +
         homclass3_V_gc*
         dclass3_V*
         wid_p90p50 +
         (homclass3_V_gc+dclass3_V|country2),data=dfreg,weights = WEIGHT)

homo3_III_wid_p90p50 <- 
  lmer(egal~ 
         homclass3_III_gc+
         know_total_gc+socialtrust_gc+
         dclass3_III+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         wid_p90p50 +loggdppercapita + rel_red + gv_spen +
         homclass3_III_gc*
         dclass3_III*
         wid_p90p50 +
         (homclass3_III_gc+dclass3_III|country2),data=dfreg,weights = WEIGHT)

homo3_III_V_wid_p90p50 <- 
  lmer(egal~ 
         homclass3_III_V_gc+
         know_total_gc+socialtrust_gc+
         dclass3_III_V+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         wid_p90p50 +loggdppercapita + rel_red + gv_spen +
         homclass3_III_V_gc*
         dclass3_III_V*
         wid_p90p50 +
         (homclass3_III_V_gc+dclass3_III_V|country2),data=dfreg,weights = WEIGHT)


inte_wid_p90p50 <-list(homo_wid_p90p50,homo2_wid_p90p50,homo2_V_wid_p90p50,homo3_wid_p90p50,homo3_V_wid_p90p50,homo3_III_wid_p90p50,homo3_III_V_wid_p90p50)

screenreg(inte_wid_p90p50,
          # "output/tables/macro_homo-p90b50_diff-operat-full.txt",
          # "output/tables/macro_homo-p90b50_diff-operat-oecd.txt",
          single.row = T)


## Ratio Share GDP top 10%---------------------------------------------------------
homo_top10 <- 
  lmer(egal~ homclass_gc+know_total_gc+socialtrust_gc+class3+female_gc+agenum_gc+age2_gc+edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+union_gc+workst_gc+top10 +loggdppercapita + rel_red + gv_spen + homclass_gc*class3*d10d1 +(homclass_gc+class3|country2),data=dfreg,weights = WEIGHT)

homo_V_top10 <- 
  lmer(egal~ 
         homclass_V_gc+
         know_total_gc+socialtrust_gc+
         class3_V+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         top10 +loggdppercapita + rel_red + gv_spen +
         homclass_V_gc*
         class3_V*
         top10 +
         (homclass_V_gc+class3_V|country2),data=dfreg,weights = WEIGHT)

homo2_top10 <- 
  lmer(egal~ 
         homclass2_gc+
         know_total_gc+socialtrust_gc+
         digclass3+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         top10 +loggdppercapita + rel_red + gv_spen +
         homclass2_gc*
         digclass3*
         top10 +
         (homclass2_gc+digclass3|country2),data=dfreg,weights = WEIGHT)

homo2_V_top10 <- 
  lmer(egal~ 
         homclass2_V_gc+
         know_total_gc+socialtrust_gc+
         digclass3_V+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         top10 +loggdppercapita + rel_red + gv_spen +
         homclass2_V_gc*
         digclass3_V*
         top10 +
         (homclass2_V_gc+digclass3_V|country2),data=dfreg,weights = WEIGHT)

homo3_top10 <- 
  lmer(egal~ 
         homclass3_gc+
         know_total_gc+socialtrust_gc+
         dclass3+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         top10 +loggdppercapita + rel_red + gv_spen +
         homclass3_gc*
         dclass3*
         top10 +
         (homclass3_gc+dclass3|country2),data=dfreg,weights = WEIGHT)

homo3_V_top10 <- 
  lmer(egal~ 
         homclass3_V_gc+
         know_total_gc+socialtrust_gc+
         dclass3_V+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         top10 +loggdppercapita + rel_red + gv_spen +
         homclass3_V_gc*
         dclass3_V*
         top10 +
         (homclass3_V_gc+dclass3_V|country2),data=dfreg,weights = WEIGHT)

homo3_III_top10 <- 
  lmer(egal~ 
         homclass3_III_gc+
         know_total_gc+socialtrust_gc+
         dclass3_III+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         top10 +loggdppercapita + rel_red + gv_spen +
         homclass3_III_gc*
         dclass3_III*
         top10 +
         (homclass3_III_gc+dclass3_III|country2),data=dfreg,weights = WEIGHT)

homo3_III_V_top10 <- 
  lmer(egal~ 
         homclass3_III_V_gc+
         know_total_gc+socialtrust_gc+
         dclass3_III_V+
         female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         top10 +loggdppercapita + rel_red + gv_spen +
         homclass3_III_V_gc*
         dclass3_III_V*
         top10 +
         (homclass3_III_V_gc+dclass3_III_V|country2),data=dfreg,weights = WEIGHT)


inte_top10 <-list(homo_top10,homo2_top10,homo2_V_top10,homo3_top10,homo3_V_top10,homo3_III_top10,homo3_III_V_top10)

screenreg(inte_top10,
          "output/tables/macro_homo-top10_diff-operat-full.txt",
          # "output/tables/macro_homo-top10_diff-operat-oecd.txt",
          single.row = T)
