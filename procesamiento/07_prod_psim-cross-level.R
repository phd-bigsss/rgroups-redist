# Power analysis 
set1 <-  RColorBrewer::brewer.pal(n = 8, name = "Set1")
options(ggplot2.discrete.fill = set1)
options(ggplot2.discrete.colour = set1)
ggplot2::theme_set(ggplot2::theme_bw())
ggplot2::theme_update(text=ggplot2::element_text(size=15,  family="serif"))
options(scipen=999)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(simr,dplyr,sjmisc,ggplot2,interplot,marginaleffects,sjlabelled,haven,stringr,ordinal,texreg,MLMusingR,lme4)  
rm(list=ls())


# setwd("~/paper1")
# load("study1_country.RData");df1 <- df2
load(here::here("input/data/proc/study1_country.RData"));df1 <- df2
# load("additional_macro_avg.RData")
load(here::here("input/data/proc/additional_macro_avg.RData"))

df1$idnum <- rownames(df1)

dfid <- df1 %>% dplyr::select(
  idnum,
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
  "gini_disp"=wid_gini_disp,
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
  country2, country, country3,
) %>% na.omit()

# Merge new macro-data
df1 <- df1 %>% left_join(.,additional_macro_avg,by ="ccode")
dfreg <- df1 %>% dplyr::select(
  idnum,
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
  "gini_disp"=wid_gini_disp,
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
  cpi,
  mig_share,
  union_den,
  v2x_polyarchy,
  unemployment_rate,
  v2catrauni,
  country2, country, country3,
) %>%
  filter(country2 != "SVN") %>% 
  # na.omit() %>%
  filter(idnum %in%  dfid$idnum) %>% 
  mutate(loggdppercapita=scale(gdppercapita/1000),
         edyears2 = edyears ^ 2,
         age2 = agenum ^ 2, 
         gini_disp=scale(gini_disp),
         gini_mkt=scale(gini_mkt)
         ) 


dfreg$wstate2 <- scale(scale(dfreg$abs_red) + scale(dfreg$ilo_taxrev) + scale(dfreg$ilo_govspe))

dfreg$wstate2 <- scale(dfreg$rel_red)

# Variable Group-Centering ------------------------------------------------$
dfreg <- 
  dfreg %>% 
  mutate(to_dummy(female),
         to_dummy(workst),
         to_dummy(Q03pcm)
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
                                workst_2,Q03pcm_1,Q03pcm_2,Q03pcm_2,Q03pcm_4,
))

library(simr)
dfreg$egal_sd <- scale(dfreg$egal) #standardized DV

homo_V_gini_wstate <- 
  lmer(egal_sd~ 
         homclass3_V_gc+
         know_total_gc+
         dclass3res_V+
         female_gc+agenum_gc+
         partner_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+         
         workst_gc+
         gini_disp+
         homclass3_V_gc*dclass3res_V*gini_disp + wstate2+ loggdppercapita +
         (homclass3_V_gc+dclass3res_V|country2),data=dfreg,weights = WEIGHT)

screenreg(homo_V_gini_wstate)

coef<- summary(homo_V_gini_wstate)$coef[,"Estimate"]
fixed<- as.numeric(coef)
varcomp<- as.data.frame(lme4::VarCorr(homo_V_gini_wstate))
varcomp$vcov[1] # var country
varcomp$sdcor[2] # var individual

# fixed intercept and slope
fixed <- fixed
# random intercept and slope variance-covariance matrix
# For class
rand <- varcomp$vcov[1]

# Exrtact the residual sd
s <- varcomp$sdcor[2] 

model <- makeLmer(egal_sd~ 
                    homclass3_V_gc+
                    know_total_gc+
                    dclass3res_V+
                    female_gc+agenum_gc+
                    partner_gc+
                    edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+         
                    workst_gc+
                    gini_disp +loggdppercapita + wstate2+
                    homclass3_V_gc*dclass3res_V*gini_disp + 
                    homclass3_V_gc*dclass3res_V*wstate2 + 
                    loggdppercapita + wstate2+ (1|country2), fixef=fixed, VarCorr=rand, sigma=s, data=dfreg)


# Fixed effects:
#                                                                    Estimate Std. Error t value
# (Intercept)                                                      -0.0669281  0.0603010  -1.110
# homclass3_V_gc                                                   -0.2743596  0.0460242  -5.961
# know_total_gc                                                    -0.0109414  0.0022103  -4.950
# dclass3res_VIntermediate class (III+IV)                           0.0819362  0.0147581   5.552
# dclass3res_VWorking Class (V+VI+VII)                              0.1321815  0.0162268   8.146
# female_gc                                                         0.0914858  0.0107632   8.500
# agenum_gc                                                         0.0034234  0.0003835   8.928
# partner_gc                                                       -0.0427024  0.0109440  -3.902
# edyears_gc                                                       -0.0041948  0.0015000  -2.797
# Q03pcm_2_gc                                                      -0.0847864  0.0148576  -5.707
# Q03pcm_3_gc                                                      -0.1793684  0.0153355 -11.696
# Q03pcm_NA_gc                                                     -0.1575464  0.0164416  -9.582
# workst_gc                                                        -0.0109195  0.0129403  -0.844
# gini_disp                                                         0.0749443  0.1354544   0.553
# loggdppercapita                                                  -0.1828484  0.0855480  -2.137
# wstate2                                                           0.1043202  0.1216127   0.858
# homclass3_V_gc:dclass3res_VIntermediate class (III+IV)            0.3162079  0.0680303   4.648  
# homclass3_V_gc:dclass3res_VWorking Class (V+VI+VII)               0.4278862  0.0620433   6.897 
# homclass3_V_gc:gini_disp                                          0.0928273  0.0935061   0.993  
# dclass3res_VIntermediate class (III+IV):gini_disp                 0.0343469  0.0277124   1.239 
# dclass3res_VWorking Class (V+VI+VII):gini_disp                    0.0047323  0.0298455   0.159
# homclass3_V_gc:wstate2                                           -0.2066162  0.0899880  -2.296
# dclass3res_VIntermediate class (III+IV):wstate2                   0.1249145  0.0275472   4.535
# dclass3res_VWorking Class (V+VI+VII):wstate2                      0.0898216  0.0295007   3.045
# homclass3_V_gc:dclass3res_VIntermediate class (III+IV):gini_disp -0.2708755  0.1435979  -1.886 <- simulate this 
# homclass3_V_gc:dclass3res_VWorking Class (V+VI+VII):gini_disp    -0.1168440  0.1226720  -0.952 <- simulate this
# homclass3_V_gc:dclass3res_VIntermediate class (III+IV):wstate2    0.1084545  0.1360306   0.797 
# homclass3_V_gc:dclass3res_VWorking Class (V+VI+VII):wstate2       0.2828655  0.1180662   2.396

model_re <- 
  lmer(egal_sd~ 
         homclass3_V_gc+
         know_total_gc+
         dclass3res_V+
         female_gc+agenum_gc+
         partner_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+         
         workst_gc+
         gini_disp +loggdppercapita + wstate2+
         homclass3_V_gc*dclass3res_V*gini_disp + 
         homclass3_V_gc*dclass3res_V*wstate2 + 
         (homclass3_V_gc+dclass3res_V|country2),data=dfreg,weights = WEIGHT)


coef<- summary(model)$coef[,"Estimate"]
fixed<- as.numeric(coef)
varcomp<- as.data.frame(lme4::VarCorr(model))
varcomp$vcov[1:4] # var country
varcomp$sdcor[2] # var individual

# fixed intercept and slope
fixed <- fixed
# random intercept and slope variance-covariance matrix
# For class
rand <- lme4::VarCorr(model)
# Exrtact the residual sd
s <- varcomp$sdcor[11] 

model <- makeLmer(egal_sd~ 
                    homclass3_V_gc+
                    know_total_gc+
                    dclass3res_V+
                    female_gc+agenum_gc+
                    partner_gc+
                    edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+         
                    workst_gc+
                    gini_disp +loggdppercapita + wstate2+
                    homclass3_V_gc*dclass3res_V*gini_disp + 
                    homclass3_V_gc*dclass3res_V*wstate2 + 
                    loggdppercapita + wstate2+ (homclass3_V_gc+dclass3res_V|country2), fixef=fixed, VarCorr=rand, sigma=s, data=dfreg)

## Simulation for N clusters ----------------------------------------------
n_clusters <- c(50,70,85,100,120) # set the number of clusters
n_sim <- 1000 # set the number of simulations


# Simulation: Homogeneity*Intermediate*Gini -------------------------------
psim_inte <- list() # set empty list 
for (i in n_clusters) {
  model_psim <- extend(model, along = "country2", n =i)
  psim_inte[[i]] <-powerSim(model_psim, 
             test=simr::fixed("homclass3_V_gc:dclass3res_VIntermediate class (III+IV):gini_disp",method = c("t")),
             nsim=n_sim,
             alpha=.05,
             progress=TRUE)
}

save(psim_inte,file = "psim_xlvl_middle.RData")

# Simulation: Homogeneity*Working*Gini ------------------------------------
psim_work <- list() # set empty list 
for (i in n_clusters) {
  model_psim <- extend(model, along = "country2", n =i)
  psim_work[[i]] <-
    powerSim(model_psim,
             test=simr::fixed("homclass3_V_gc:dclass3res_VWorking Class (V+VI+VII):gini_disp",method = c("t")),
             nsim=n_sim,
             alpha=.05,
             progress=TRUE)
}

save(psim_inte,file = "psim_xlvl_work.RData")