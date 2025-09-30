set1 <-  RColorBrewer::brewer.pal(n = 8, name = "Set1")
options(ggplot2.discrete.fill = set1)
options(ggplot2.discrete.colour = set1)
ggplot2::theme_set(ggplot2::theme_bw())
ggplot2::theme_update(text=ggplot2::element_text(size=15,  family="serif"))
options(scipen=999)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,sjmisc,sjlabelled,haven,vtable)  
rm(list=ls())
load(here::here("input/data/proc/study1_country.RData"));df1 <- df2




df1$isolation_v1<- 
  sjmisc::rec(set_na(car::recode(df1$v31,"c(8,9)=NA"),na = c(0,8,9)),rec="rev");sjmisc::frq(df1$isolation_v1)
df1$isolation_v2<- 
  sjmisc::rec(set_na(car::recode(df1$v32,"c(8,9)=NA"),na = c(0,8,9)),rec="rev");sjmisc::frq(df1$isolation_v2)
df1$isolation_v3<- 
  sjmisc::rec(set_na(car::recode(df1$v33,"c(8,9)=NA"),na = c(0,8,9)),rec="rev");sjmisc::frq(df1$isolation_v3)


df1$part_v1<- 
  sjmisc::rec(set_na(car::recode(df1$v17,"c(8,9)=NA"),na = c(0,8,9)),rec="rev");sjmisc::frq(df1$part_v1)
df1$part_v2<- 
  sjmisc::rec(set_na(car::recode(df1$v18,"c(8,9)=NA"),na = c(0,8,9)),rec="rev");sjmisc::frq(df1$part_v2)
df1$part_v3<- 
  sjmisc::rec(set_na(car::recode(df1$v19,"c(8,9)=NA"),na = c(0,8,9)),rec="rev");sjmisc::frq(df1$part_v3)


sjmisc::frq(df1$VOTE_LE)
df1$vote <- factor(car::recode(df1$VOTE_LE,recodes = "1=1;2=0;else=NA"))
table(df1$country3,df1$vote)

df1$politrust <- set_na(car::recode(df1$v36,"c(98,99)=NA"),na = c(98,99));frq(df1$politrust)

dfreg <- df1 %>% dplyr::select(
  socialtrust,
  politrust,
  part_v1,part_v2,part_v3,
  vote,
  isei08=isei08,
  # dclass3spo_V,
  segregation=socdist,
  # know_total,
  Q03pcm,
  edyears,
  female,
  agenum,
  # religion,
  # partner,
  # union,
  workst,
  "gini_disp"=wid_gini_disp,
  # "gini_disp"=gini_disp,
  abs_red,
  ilo_taxrev,
  ilo_govspe,
  rel_red,
  d10d1=wid_p90p10,
  wid_p90p50,
  top10=wid_sharetop10,
  gdppercapita,
  country2,country3,country,oecd,
) %>%
  # filter(oecd == "OECD") %>%
  # na.omit() %>% 
  mutate(loggdppercapita=log(gdppercapita/1000),
         edyears2 = edyears ^ 2,
         age2 = agenum ^ 2, 
         # gini_disp=scale(gini_disp)
  ) 

frq(dfreg$country3)

haven::write_dta(data = dfreg,path = "paper_segregacion-cohesion.dta")

# Use car::recode to classify countries into welfare regimes
dfreg$wregime <- car::recode(var = dfreg$country3,as.factor = T,
                             recodes = 
  "'Australia' = 'Liberal';
   'New Zealand' = 'Liberal';
   'United Kingdom' = 'Liberal';
   'United States' = 'Liberal';
   
   'Austria' = 'Conservative';
   'Croatia' = 'Conservative';
   'France' = 'Conservative';
   'Germany' = 'Conservative';
   'Spain' = 'Conservative';
   'Switzerland' = 'Conservative';
   
   'Czechia' = 'Post-Socialist';
   'Hungary' = 'Post-Socialist';
   'Estonia' = 'Post-Socialist';
   'Lithuania' = 'Post-Socialist';
   'Slovakia' = 'Post-Socialist';
   'Slovenia' = 'Post-Socialist';
   'Russia' = 'Post-Socialist';   
   
   'Denmark' = 'Social Democratic';
   'Finland' = 'Social Democratic';
   'Iceland' = 'Social Democratic';
   'Sweden' = 'Social Democratic';
   
   'China' = 'East Asian';
   'Japan' = 'East Asian';
   'Taiwan' = 'East Asian';
   'Thailand' = 'East Asian';
   
   'Mexico' = 'Latin American';
   'Suriname' = 'Latin American';
   
   'Israel' = 'Other'; 
   'Turkey' = 'Other';
   'South Africa' = 'Other';
   'Philippines' = 'Other';
   'India' = 'Other';
   ")

# omit_countries<- c("Mexico","Suriname","South Africa","India","China","Philippines","China")

# dfreg<- dfreg %>% filter(!(country3 %in% omit_countries))

frq(dfreg$wregime)
levels(dfreg$wregime)

dfreg$wregime <- factor(dfreg$wregime,
                        levels = c("Liberal",
                                   "East Asian","Latin American",
                                   "Conservative",
                                   "Post-Socialist",
                                   "Social Democratic","Hybrid","Other"))

summary(dfreg)
# dfreg$wstate2 <- scale(scale(dfreg$abs_red*100) + scale(dfreg$ilo_taxrev) + scale(dfreg$ilo_govspe))

dfreg$wstate<- rowMeans(dfreg[,c("abs_red","ilo_taxrev","ilo_govspe")],na.rm = T)
dfreg$wstate2 <- ((dfreg$wstate-min(dfreg$wstate,na.rm = T))/(max(dfreg$wstate,na.rm = T)-min(dfreg$wstate,na.rm = T)))*100


dfreg$wstate2 <- scale(dfreg$wstate2)
dfreg$loggdppercapita <- scale(dfreg$loggdppercapita)
dfreg$gini_disp <- scale(dfreg$gini_disp)

# Step 1: Calculate absolute values
summary(dfreg$segregation)
88 - 75 # Ego es mas alto que la red
75 - 88 # Ego es más bajo que la red

absolute_values <- abs(dfreg$segregation)
hist(absolute_values);summary(absolute_values)

# Step 2: Add 1 to all values to eliminate zeros
modified_values <- absolute_values + 1 # then in in this distribution 1= homogeneity;else = heterogeneity
hist(modified_values);summary(modified_values)

# Step 3: Reverse the order of the scale, higher values represent greater homogeneity in the network
reversed_values <- max(modified_values,na.rm = T)  -  modified_values
hist(reversed_values);summary(reversed_values)
dfreg$segabs <- reversed_values/10
hist(dfreg$segabs)

homo_ineq <- dfreg %>%
  group_by(country2,country3) %>%
  summarise_at(c("socialtrust",
                 "isolation_v1","isolation_v2","isolation_v3",
                 "part_v1","part_v2","part_v3",
                 "segregation","segabs","wstate2",
                 "abs_red","rel_red",
                 "ilo_taxrev",
                 "ilo_govspe"), mean, na.rm = TRUE) %>% 
  as.data.frame()

correlation::correlation(homo_ineq) %>% summary()

library(lme4);library(texreg);library(jtools)
dfreg$segabs_gc <- MLMusingR::group_center(dfreg$segabs,grp = dfreg$country2)

# dfreg$isolation <- (dfreg$isolation_v1+dfreg$isolation_v2+dfreg$isolation_v3)/3
dfreg$participation <- (dfreg$part_v1+dfreg$part_v2+dfreg$part_v3)/3

cohesion_null <-
  list(
    soctrust=lmer(socialtrust~1+(1|country2),data = dfreg),
    poltrust=lmer(politrust~1 +(1|country2),data = dfreg),
    socparti=lmer(participation~1+(1|country2),data = dfreg),
    vote=lmer(as.numeric(vote)~1+(1|country2),data = dfreg)
  )

performance::icc(cohesion_null$soctrust)
performance::icc(cohesion_null$poltrust)
performance::icc(cohesion_null$socparti)
performance::icc(cohesion_null$vote)

cohesion_segregation_rs <-
  list(
    soctrust=lmer(socialtrust~segabs+isei08+female+agenum+workst+Q03pcm+edyears+(segabs|country2),data = dfreg),
    poltrust=lmer(politrust~segabs+isei08+female+agenum+workst+Q03pcm+edyears+(segabs|country2),data = dfreg),
    socparti=lmer(participation~segabs+isei08+female+agenum+workst+Q03pcm+edyears+(segabs|country2),data = dfreg),
    vote=lmer(as.numeric(vote)~segabs+isei08+female+agenum+workst+Q03pcm+edyears+(segabs|country2),data = dfreg)
  )


sjPlot::plot_model(cohesion_segregation_rs$soctrust,type = "re")

sjPlot::plot_model(cohesion_segregation_rs$poltrust,type = "re")

sjPlot::plot_model(cohesion_segregation_rs$socparti,type = "re")

sjPlot::plot_model(cohesion_segregation_rs$vote,type = "re")



# Models: social cohesion, welfare state (country-level)  --------------------------------
cohesion_ws <-
list(
  soctrust=lmer(socialtrust~segabs+isei08+female+agenum+workst+Q03pcm+edyears+wstate2+loggdppercapita+(1|country2),data = dfreg),
  poltrust=lmer(politrust~segabs+isei08+female+agenum+workst+Q03pcm+edyears+wstate2 +loggdppercapita+  (1|country2),data = dfreg),
  socparti=lmer(participation~segabs+isei08+female+agenum+workst+Q03pcm+edyears+wstate2 + loggdppercapita+(1|country2),data = dfreg),
  vote=lmer(as.numeric(vote)~segabs+isei08+female+agenum+workst+Q03pcm+edyears+wstate2+loggdppercapita+(1|country2),data = dfreg)
  )

screenreg(cohesion_ws)

cohesion_ws_int <- 
  list(
    soctrust=lmer(socialtrust~segabs_gc*wstate2+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    poltrust=lmer(politrust~segabs_gc*wstate2+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    socparti=lmer(participation~segabs_gc*wstate2+isei08+loggdppercapita+female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    vote=lmer(as.numeric(vote)~segabs_gc*wstate2+isei08+female+agenum+workst+Q03pcm+edyears+loggdppercapita+(segabs_gc|country2),data = dfreg)
  ) 

screenreg(cohesion_ws_int)

# Social cohesion, segregation, welfare regimes  --------------------------

cohesion_wregimes <-
  list(
    soctrust=lmer(socialtrust~segabs+isei08+female+agenum+workst+Q03pcm+edyears+wregime+(1|country2),data = dfreg),
    poltrust=lmer(politrust~segabs+isei08+female+agenum+workst+Q03pcm+edyears+wregime+(1|country2),data = dfreg),
    socparti=lmer(participation~segabs+isei08+female+agenum+workst+Q03pcm+edyears+wregime+(1|country2),data = dfreg),
    vote=lmer(as.numeric(vote)~segabs+isei08+female+agenum+workst+Q03pcm+edyears+wregime+(1|country2),data = dfreg)
  )

screenreg(cohesion_wregimes)

cohesion_seg_wregimes <-
  list(
    soctrust=lmer(socialtrust~segabs_gc+isei08+female+agenum+workst+Q03pcm+edyears+segabs_gc*wregime+(segabs|country2),data = dfreg),
    poltrust=lmer(politrust~segabs_gc+isei08+female+agenum+workst+Q03pcm+edyears+segabs_gc*wregime+(segabs|country2),data = dfreg),
    socparti=lmer(participation~segabs_gc+isei08+female+agenum+workst+Q03pcm+edyears+segabs_gc*wregime+(segabs|country2),data = dfreg),
    vote=lmer(as.numeric(vote)~segabs_gc+isei08+female+agenum+workst+Q03pcm+edyears+segabs_gc*wregime+(segabs|country2),data = dfreg)
  )

screenreg(cohesion_seg_wregimes)

# Absolute redistribution ---------------------------------------------------------------
cohesion_ws_int1 <- 
  list(
    soctrust=lmer(socialtrust~segabs_gc*abs_red+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    poltrust=lmer(politrust~segabs_gc*abs_red+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    socparti=lmer(participation~segabs_gc*abs_red+isei08++loggdppercapita+female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    vote=lmer(as.numeric(vote)~segabs_gc*abs_red+isei08+female+agenum+workst+Q03pcm+edyears+loggdppercapita+(segabs_gc|country2),data = dfreg)
  ) 
screenreg(cohesion_ws_int1)


# Relative redistribution ---------------------------------------------------------------
cohesion_ws_int1 <- 
  list(
    soctrust=lmer(socialtrust~segabs_gc*rel_red+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    poltrust=lmer(politrust~segabs_gc*rel_red+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    socparti=lmer(participation~segabs_gc*rel_red+isei08++loggdppercapita+female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    vote=lmer(as.numeric(vote)~segabs_gc*rel_red+isei08+female+agenum+workst+Q03pcm+edyears+loggdppercapita+(segabs_gc|country2),data = dfreg)
  ) 
screenreg(cohesion_ws_int1)


# Tax reveneu as pct of GDP ---------------------------------------------------------------
cohesion_ws_int2 <- 
  list(
    soctrust=lmer(socialtrust~segabs_gc*ilo_taxrev+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    poltrust=lmer(politrust~segabs_gc*ilo_taxrev+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    socparti=lmer(participation~segabs_gc*ilo_taxrev+isei08++loggdppercapita+female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    vote=lmer(as.numeric(vote)~segabs_gc*ilo_taxrev+isei08+female+agenum+workst+Q03pcm+edyears+loggdppercapita+(segabs_gc|country2),data = dfreg)
  ) 
screenreg(cohesion_ws_int2)

# Government spending as pct of GDP ---------------------------------------------------------------

cohesion_ws_int3 <- 
  list(
    soctrust=lmer(socialtrust~segabs_gc*ilo_govspe+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    poltrust=lmer(politrust~segabs_gc*ilo_govspe+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    socparti=lmer(participation~segabs_gc*ilo_govspe+isei08++loggdppercapita+female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    vote=lmer(as.numeric(vote)~segabs_gc*ilo_govspe+isei08+female+agenum+workst+Q03pcm+edyears+loggdppercapita+(segabs_gc|country2),data = dfreg)
  ) 

screenreg(cohesion_ws_int3)


# Segregation+Gini --------------------------------------------------------
cohesion_gini <- 
  list(
    soctrust=lmer(socialtrust~segabs_gc+gini_disp+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    poltrust=lmer(politrust~segabs_gc+gini_disp+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    socparti=lmer(participation~segabs_gc+gini_disp+isei08+loggdppercapita+female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    vote=lmer(as.numeric(vote)~segabs_gc+gini_disp+isei08+female+agenum+workst+Q03pcm+edyears+loggdppercapita+(segabs_gc|country2),data = dfreg)
  ) 

screenreg(cohesion_gini)

cohesion_gini_int <- 
  list(
    soctrust=lmer(socialtrust~segabs_gc*gini_disp+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    poltrust=lmer(politrust~segabs_gc*gini_disp+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    socparti=lmer(participation~segabs_gc*gini_disp+isei08+loggdppercapita+female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    vote=lmer(as.numeric(vote)~segabs_gc*gini_disp+isei08+female+agenum+workst+Q03pcm+edyears+loggdppercapita+(segabs_gc|country2),data = dfreg)
  ) 

screenreg(cohesion_gini_int)

# segregation+ratio90/10 --------------------------------------------------

cohesion_ws_int4 <- 
  list(
    soctrust=lmer(socialtrust~segabs_gc+d10d1+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    poltrust=lmer(politrust~segabs_gc+d10d1+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    socparti=lmer(participation~segabs_gc+d10d1+isei08+loggdppercapita+female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    vote=lmer(as.numeric(vote)~segabs_gc+d10d1+isei08+female+agenum+workst+Q03pcm+edyears+loggdppercapita+(segabs_gc|country2),data = dfreg)
  ) 

screenreg(cohesion_ws_int4)
# segregation*ratio90/10 --------------------------------------------------
cohesion_ws_int4_int <- 
  list(
    soctrust=lmer(socialtrust~segabs_gc*d10d1+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    poltrust=lmer(politrust~segabs_gc*d10d1+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    socparti=lmer(participation~segabs_gc*d10d1+isei08+loggdppercapita+female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    vote=lmer(as.numeric(vote)~segabs_gc*d10d1+isei08+female+agenum+workst+Q03pcm+edyears+loggdppercapita+(segabs_gc|country2),data = dfreg)
  ) 

screenreg(cohesion_ws_int4_int)
# segregation*top10pct --------------------------------------------------

cohesion_ws_int5 <- 
  list(
    soctrust=lmer(socialtrust~segabs_gc+top10+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg),
    poltrust=lmer(politrust~segabs_gc+top10+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg),
    socparti=lmer(participation~segabs_gc+top10+isei08+loggdppercapita+female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg),
    vote=lmer(as.numeric(vote)~segabs_gc+top10+isei08+female+agenum+workst+Q03pcm+edyears+loggdppercapita+(1|country2),data = dfreg)
  )
screenreg(cohesion_ws_int5)

cohesion_ws_int5_int <- 
  list(
    soctrust=lmer(socialtrust~segabs_gc*top10+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    poltrust=lmer(politrust~segabs_gc*top10+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    socparti=lmer(participation~segabs_gc*top10+isei08++loggdppercapita+female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    vote=lmer(as.numeric(vote)~segabs_gc*top10+isei08+female+agenum+workst+Q03pcm+edyears+loggdppercapita+(segabs_gc|country2),data = dfreg)
  )

screenreg(cohesion_ws_int5_int)

# segregation*top90/bottom50 --------------------------------------------------

cohesion_ws_int6 <- 
  list(
    soctrust=lmer(socialtrust~segabs_gc+wid_p90p50+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg),
    poltrust=lmer(politrust~segabs_gc+wid_p90p50+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg),
    socparti=lmer(participation~segabs_gc+wid_p90p50+isei08+loggdppercapita+female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg),
    vote=lmer(as.numeric(vote)~segabs_gc+wid_p90p50+isei08+female+agenum+workst+Q03pcm+edyears+loggdppercapita+(1|country2),data = dfreg)
  )

screenreg(cohesion_ws_int6)

cohesion_ws_int6_int <- 
  list(
    soctrust=lmer(socialtrust~segabs_gc*wid_p90p50+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    poltrust=lmer(politrust~segabs_gc*wid_p90p50+isei08+loggdppercapita +female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    socparti=lmer(participation~segabs_gc*wid_p90p50+isei08+loggdppercapita+female+agenum+workst+Q03pcm+edyears+(segabs_gc|country2),data = dfreg),
    vote=lmer(as.numeric(vote)~segabs_gc*wid_p90p50+isei08+female+agenum+workst+Q03pcm+edyears+loggdppercapita+(segabs_gc|country2),data = dfreg)
  )

screenreg(cohesion_ws_int6_int)


cohesion_seg_isei <- 
  list(
    soctrust=lmer(socialtrust~isei08*segabs+female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg),
    poltrust=lmer(politrust~isei08*segabs+female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg),
    socparti=lmer(participation~isei08*segabs+female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg),
    vote=lmer(as.numeric(vote)~isei08*segabs+female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg)
  )

screenreg(cohesion_seg_isei)



cohesion_seg_isei_socialdemocratic <- 
  list(
    soctrust=lmer(socialtrust~isei08*segabs+female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg,subset = wregime =="Social Democratic"),
    poltrust=lmer(politrust~isei08*segabs+female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg,subset = wregime =="Social Democratic"),
    socparti=lmer(participation~isei08*segabs+female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg,subset = wregime =="Social Democratic"),
    vote=lmer(as.numeric(vote)~isei08*segabs+female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg,subset = wregime =="Social Democratic")
  )

screenreg(cohesion_seg_isei_socialdemocratic)

cohesion_seg_isei_conservative <- 
  list(
    soctrust=lmer(socialtrust~isei08*segabs+female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg,subset = wregime =="Conservative"),
    poltrust=lmer(politrust~isei08*segabs+female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg,subset = wregime =="Conservative"),
    socparti=lmer(participation~isei08*segabs+female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg,subset = wregime =="Conservative"),
    vote=lmer(as.numeric(vote)~isei08*segabs+female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg,subset = wregime =="Conservative")
  )

screenreg(cohesion_seg_isei_conservative)

cohesion_seg_isei_liberal <- 
  list(
    soctrust=lmer(socialtrust~isei08*segabs+female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg,subset = wregime =="Liberal"),
    poltrust=lmer(politrust~isei08*segabs+female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg,subset = wregime =="Liberal"),
    socparti=lmer(participation~isei08*segabs+female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg,subset = wregime =="Liberal"),
    vote=lmer(as.numeric(vote)~isei08*segabs+female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg,subset = wregime =="Liberal")
  )

screenreg(cohesion_seg_isei_liberal)

cohesion_seg_isei_latam <- 
  list(
    soctrust=lmer(socialtrust~isei08*segabs+female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg,subset = wregime =="Latin American"),
    poltrust=lmer(politrust~isei08*segabs+female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg,subset = wregime =="Latin American"),
    socparti=lmer(participation~isei08*segabs+female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg,subset = wregime =="Latin American"),
    vote=lmer(as.numeric(vote)~isei08*segabs+female+agenum+workst+Q03pcm+edyears+(1|country2),data = dfreg,subset = wregime =="Latin American")
  )

screenreg(cohesion_seg_isei_latam)



library(marginaleffects)


plot_slopes(cohesion_seg_isei$soctrust, variables = "isei08",by = "segabs")

# # Models: social cohesion, segregation (country-level) ------------------------------------
# quantile(x = dfreg$segabs,na.rm = T,probs = c(0,0.1,0.2,0.3,0.4,0.5,0.6))
# 
# dfreg<- dfreg %>% group_by(country2) %>% mutate(seg_gm=mean(segabs,na.rm=T))
# dfreg$seg_gm <- scale(dfreg$seg_gm)
# 
# 
# cohesion_seg<- 
# list(
#   soctrust=lmer(socialtrust~isei08+female+agenum+workst+Q03pcm+edyears+seg_gm+loggdppercapita+(1|country2),data = dfreg),
#   poltrust=lmer(politrust~isei08+female+agenum+workst+Q03pcm+edyears+seg_gm+loggdppercapita+(1|country2),data = dfreg),
#   isolatio=lmer(isolation~isei08+female+agenum+workst+Q03pcm+edyears+seg_gm+loggdppercapita+(1|country2),data = dfreg),
#   socparti=lmer(participation~isei08+female+agenum+workst+Q03pcm+edyears+seg_gm+loggdppercapita+(1|country2),data = dfreg),
#   vote=glmer(vote~isei08+female+agenum+workst+Q03pcm+edyears+seg_gm+loggdppercapita+(1|country2),data = dfreg,family = "binomial")
# ) 
# 
# screenreg(cohesion_seg)
# 
# dfreg$isei08_gc <- MLMusingR::group_center(dfreg$isei08,grp = dfreg$country2)
# cohesion_seg_int <-
# list(
#   soctrust=lmer(socialtrust~isei08_gc+female+agenum+workst+Q03pcm+edyears+isei08_gc*seg_gm+(isei08_gc|country2),data = dfreg),
#   poltrust=lmer(politrust~isei08_gc+female+agenum+workst+Q03pcm+edyears+isei08_gc*seg_gm+(isei08_gc|country2),data = dfreg),
#   isolatio=lmer(isolation~isei08_gc+female+agenum+workst+Q03pcm+edyears+isei08_gc*seg_gm+(isei08_gc|country2),data = dfreg),
#   socparti=lmer(participation~isei08_gc+female+agenum+workst+Q03pcm+edyears+isei08_gc*seg_gm+(isei08_gc|country2),data = dfreg),
#   vote=glmer(vote~isei08_gc+female+agenum+workst+Q03pcm+edyears+isei08_gc*seg_gm+(isei08_gc|country2),data = dfreg,family = "binomial")
# ) 
# 
# screenreg(cohesion_seg_int)
# 
# library(marginaleffects)
# library(ggplot2)
# plot_predictions(cohesion_seg_int[["soctrust"]], condition = list("seg_gm","isei08_gc"="threenum")) 
# plot_slopes(cohesion_seg_int[["soctrust"]], variables = "isei08_gc", condition = "seg_gm") + geom_hline(yintercept = 0,color="red",linetype="dashed") 

