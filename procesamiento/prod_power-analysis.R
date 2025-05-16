# Power analysis 
set1 <-  RColorBrewer::brewer.pal(n = 8, name = "Set1")
options(ggplot2.discrete.fill = set1)
options(ggplot2.discrete.colour = set1)
ggplot2::theme_set(ggplot2::theme_bw())
ggplot2::theme_update(text=ggplot2::element_text(size=15,  family="serif"))
options(scipen=999)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,sjmisc,ggplot2,interplot,marginaleffects,sjlabelled,haven,stringr,ordinal,texreg,MLMusingR,lme4)  
# rm(list=ls()) 

load(here::here("input/data/proc/study1_country.RData"));df1 <- df2
load(file ="input/data/proc/additional_macro_avg.RData")

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
  mutate(loggdppercapita=scale(gdppercapita/1000),
         edyears2 = edyears ^ 2,
         age2 = agenum ^ 2, 
         gini_disp=scale(gini_disp)) %>% 
  filter(idnum %in%  dfid$idnum)

dfreg$wstate2 <- scale(scale(dfreg$abs_red) + scale(dfreg$ilo_taxrev) + scale(dfreg$ilo_govspe))

library(simr)

homo_V_gini_gdp_wstate2 <- 
  lmer(egal~ 
         gini_disp +
         homclass3_V_gc+
         know_total_gc+
         dclass3res_V+
         female_gc+agenum_gc+
         partner_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         workst_gc+
         gini_disp +loggdppercapita + wstate2+
         (1|country2),data=dfreg,weights = WEIGHT)

coef<- summary(homo_V_gini_gdp_wstate2)$coef[,"Estimate"]
fixed<- as.numeric(coef)
varcomp<- as.data.frame(lme4::VarCorr(homo_V_gini_gdp_wstate2))
varcomp$vcov[1] # var country
varcomp$sdcor[2] # var individual

# fixed intercept and slope
fixed <- fixed
# random intercept and slope variance-covariance matrix
# For class
rand <- varcomp$vcov[1]

# Exrtact the residual sd
s <- varcomp$sdcor[2] 

model <- makeLmer(egal~
                    gini_disp +            
                    homclass3_V_gc+
                    know_total_gc+
                    dclass3res_V+
                    female_gc+agenum_gc+
                    partner_gc+
                    edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
                    workst_gc+
                    loggdppercapita + wstate2+ (1|country2), fixef=fixed, VarCorr=rand, sigma=s, data=dfreg)
model


# For a range of effects sizes 2.2, + 1SD, +2SD, -1SD, -2 SD 


model_ext_country <- extend(model, along = "country2", n =200)

p_sim_gini_disp <- 
  powerSim(model_ext_country, 
           test=fixed("gini_disp",method = c("t")),
           # along="country2",
           # breaks = c(10,20,30,40,50,60,70,90,100),
           nsim=1,
           alpha=.05,
           progress=TRUE);p_sim_gini_disp



p_curve_gini_disp <- powerCurve(model_ext_country, 
                                test=fixed("gini_disp",method = c("t")), 
                                along="country2",
                                breaks = c(10,20,30,40,50,60,70,90,100),
                                nsim=5*60,
                                alpha=.05,
                                progress=TRUE)

# 
# 
# - determine effect sizes (small, medium, high)
# - determine number of clusters
# - Simulate with 1000, 5000 and 10.000 reps
