set1 <-  RColorBrewer::brewer.pal(n = 8, name = "Set1")
options(ggplot2.discrete.fill = set1)
options(ggplot2.discrete.colour = set1)
ggplot2::theme_set(ggplot2::theme_bw())
ggplot2::theme_update(text=ggplot2::element_text(size=15,  family="serif"))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,sjmisc,ggplot2,interplot,marginaleffects,sjlabelled,haven,stringr,ordinal,texreg,MLMusingR)  
rm(list=ls())

load(here::here("input/data/proc/study1_country.RData"));df1 <- df2
dfreg <- df1 %>% dplyr::select(
  egal = egal2,
  class3,
  # class3=digclass3,
  # class3=dclass3,
  homclass,
  # homclass=homclass2,
  # homclass=homclass3,
  # know_total=week_cont,
  class6,
  know_total,
  Q03pcm,
  edyears,
  female,
  agenum,
  union,
  workst,
  WEIGHT,
  region,
  "gini_disp",
  "gini_mkt",
  gini,
  gv_spen,
  rel_red,
  d10d1,
  s80s20,
  top10,
  palmaratio,
  rgdpna,
  gdppercapita,
  oecd,
  country2, country
) %>%
  mutate(logrgdpna = log(rgdpna),
         loggdppercapita=log(gdppercapita),
         know_total=log(know_total),
         edyears2 = edyears ^ 2,
         age2 = agenum ^ 2) %>%
  filter(country2 != "SVN") %>% 
  # filter(country2 != "ZAF") %>%
  # filter(country2 != "HUN") %>% 
  # filter(oecd == "OECD") %>%
  na.omit()

dfreg <- bind_cols(dfreg,sjmisc::to_dummy(x = dfreg$class3,suffix = "numeric")) %>% 
  rename(prop_serv=class3_1,prop_inte=class3_2,prop_work=class3_3)
dfreg <- 
dfreg %>% 
  group_by(country2) %>% 
  mutate(prop_serv=mean(prop_serv),prop_inte=mean(prop_inte),prop_work=mean(prop_work))

dfreg <- as.data.frame(dfreg)

dfreg_country <- 
dfreg %>% 
  group_by(country2) %>% 
  summarise_at(.vars = c("homclass","egal","gini_disp","gini_mkt",
                         "rel_red","loggdppercapita"),
               .funs = mean) %>% 
  dplyr::select(-country2) 
  
# sjPlot::tab_corr(dfreg_country,triangle = "lower")
# GGally::ggpairs(dfreg_country, lower=list(continuous="smooth"), diag = list(continuous = NULL))

# sjPlot::plot_grpfrq(df1$homclass_wght,var.grp = df1$class11d,type = "boxplot",ylim = c(0,0.8))
# sjPlot::plot_grpfrq(df1$homclass,var.grp = df1$class11d,type = "boxplot",ylim = c(0,0.8))
# sjPlot::plot_model(model = lmer(homclass~factor(class11d)+edyears+Q03pcm+female+(1|country2),data = dfreg),type = "pred",terms = "class11d")
# sjPlot::plot_model(model = lmer(homclass_wght~factor(class11d)+edyears+Q03pcm+female+(1|country2),data = dfreg),type = "pred",terms = "class11d")

# cowplot::plot_grid(
#   sjPlot::plot_model(model = lm(homclass~class6+edyears+Q03pcm+female+know_total,data = subset(x = dfreg,dfreg$country2=="SWE")),type = "pred",terms = "class6",title = "Sweden"),
#   sjPlot::plot_model(model = lm(homclass~class6+edyears+Q03pcm+female+know_total,data = subset(x = dfreg,dfreg$country2=="DEU")),type = "pred",terms = "class6",title = "Germany"),
#   sjPlot::plot_model(model = lm(homclass~class6+edyears+Q03pcm+female+know_total,data = subset(x = dfreg,dfreg$country2=="GBR")),type = "pred",terms = "class6",title = "Great Britain"),
#   sjPlot::plot_model(model = lm(homclass~class6+edyears+Q03pcm+female+know_total,data = subset(x = dfreg,dfreg$country2=="USA")),type = "pred",terms = "class6",title = "United States")
# )

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
dfreg$homclass_gc = group_center(dfreg$homclass, grp = dfreg$country2)
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

# dfreg$homclass<- dfreg$homclass2

# Micro level models -----------------------------------------------------------
base <- lmer(egal~1 + (1|country2),data=dfreg,weights = WEIGHT); icc1<- performance::icc(base)
homclass<- update(base, . ~ . +homclass)
homclass_know_total<- update(homclass, . ~ . +know_total)
full1 <- update(homclass_know_total, . ~ . + class3+female+agenum+age2)
rob1 <- update(full1, . ~ . + class3+edyears+ Q03pcm+union+workst)
rob2 <- update(rob1, . ~ . -(1|country2) + (homclass|country2))
rob3 <- update(rob1, . ~ . -(1|country2) + prop_work +(homclass|country2))
rob4 <- update(rob3, . ~ . -(1|country2) + prop_work + prop_serv + (homclass|country2))

# anova(rob1,rob2)
# Interactions segregation x class
int_homo <- update(rob4, . ~ . +class3*homclass)

models <- list(homclass,homclass_know_total,full1,rob1,rob3,rob4,int_homo)
knitreg(models)

fit_homclass <-
  lmer(homclass~1 +class3+female+agenum+age2 +
         edyears+ Q03pcm+union+workst +
         prop_work + prop_inte + 
         (class3|country2),data=dfreg,weights = WEIGHT)
fit_homclass_giniM <- update(fit_homclass, . ~ . +class3*gini_mkt+loggdppercapita+rel_red)
fit_homclass_giniD <- update(fit_homclass, . ~ . +class3*gini_disp+loggdppercapita+rel_red)
fit_homclass_d10d1 <- update(fit_homclass, . ~ . +class3*d10d1+loggdppercapita+rel_red)
fit_homclass_palma <- update(fit_homclass, . ~ . +class3*palmaratio+loggdppercapita+rel_red)
fit_homclass_s80s20<- update(fit_homclass, . ~ .+class3*s80s20+loggdppercapita+rel_red)
fit_homclass_top10 <- update(fit_homclass, . ~ . +class3*top10+loggdppercapita+rel_red)

knitreg(list(fit_homclass,fit_homclass_giniM,
             fit_homclass_giniD,fit_homclass_d10d1,fit_homclass_palma,
             fit_homclass_s80s20,fit_homclass_top10))

# knitreg(list(fit_homclass,fit_homclass_giniM,
#              fit_homclass_giniD,fit_homclass_d10d1,fit_homclass_palma,
#              fit_homclass_s80s20,fit_homclass_top10),file = "output/tables/int_class3_ineq_full.txt")

# sjPlot::plot_model(fit_homclass_giniM,type = "pred",terms = c("gini_mkt", "class3")) +
#   labs(x="Gini (Market)")+
#   scale_fill_manual(values=c("#377EB8","#4DAF4A","#E41A1C")) +
#   scale_color_manual(values=c("#377EB8","#4DAF4A","#E41A1C")) + theme(legend.position = "bottom")
# sjPlot::plot_model(fit_homclass_giniD,type = "pred",terms = c("gini_disp", "class3"))+
#   labs(x="Gini (Disposable)")+
#   scale_fill_manual(values=c("#377EB8","#4DAF4A","#E41A1C")) +
#   scale_color_manual(values=c("#377EB8","#4DAF4A","#E41A1C")) + theme(legend.position = "bottom")
# sjPlot::plot_model(fit_homclass_d10d1,type = "pred",terms = c("d10d1", "class3"))+ 
#   scale_fill_manual(values=c("#377EB8","#4DAF4A","#E41A1C")) + 
#   scale_color_manual(values=c("#377EB8","#4DAF4A","#E41A1C")) + theme(legend.position = "bottom")
# sjPlot::plot_model(fit_homclass_palma,type = "pred",terms = c("palmaratio", "class3"))+ 
#   scale_fill_manual(values=c("#377EB8","#4DAF4A","#E41A1C")) + 
#   scale_color_manual(values=c("#377EB8","#4DAF4A","#E41A1C")) + theme(legend.position = "bottom")
# sjPlot::plot_model(fit_homclass_s80s20,type = "pred",terms = c("s80s20", "class3"))+ 
#   scale_fill_manual(values=c("#377EB8","#4DAF4A","#E41A1C")) + 
#   scale_color_manual(values=c("#377EB8","#4DAF4A","#E41A1C")) + theme(legend.position = "bottom")
# sjPlot::plot_model(fit_homclass_top10,type = "pred",terms = c("top10", "class3"))+ 
#   scale_fill_manual(values=c("#377EB8","#4DAF4A","#E41A1C")) + 
#   scale_color_manual(values=c("#377EB8","#4DAF4A","#E41A1C")) + theme(legend.position = "bottom")



# Interacciones cross-level -----------------------------------------------
## Gini Market--------------------------------------------------------------
base_giniM_gc <- 
  lmer(egal~homclass_gc+know_total_gc+class3+female_gc+agenum_gc+age2_gc+
         edyears_gc +Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         gini_mkt +loggdppercapita + 
         (1|country2),data=dfreg,weights = WEIGHT)
int_homo_giniM_gc <- 
  update(base_giniM_gc, . ~ . 
         +homclass_gc*class3*gini_mkt -(1|country2) +
           (homclass_gc+class3|country2))

knitreg(list(base_giniM_gc,int_homo_giniM_gc))

## Gini Disposable---------------------------------------------------------
base_giniD_gc <- 
  lmer(egal~homclass_gc+know_total_gc+class3+female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         gini_disp +loggdppercapita + rel_red +
         (1|country2),data=dfreg,weights = WEIGHT)

int_homo_giniD_gc <- 
  update(base_giniD_gc, . ~ . 
         +homclass_gc*class3*gini_disp -(1|country2) +
           (homclass_gc+class3|country2))

knitreg(list(base_giniD_gc,int_homo_giniD_gc))

# Ratio d10d1 -------------------------------------------------------------
base_d01d10_gc <- 
  lmer(egal~homclass_gc+know_total_gc+class3+female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         d10d1 +loggdppercapita + rel_red +
         (1|country2),data=dfreg,weights = WEIGHT)

int_homo_d10d1_gc <- 
  update(base_d01d10_gc, . ~ . 
         +homclass_gc*class3*d10d1 -(1|country2) +
           (homclass_gc+class3|country2))

knitreg(list(base_d01d10_gc,int_homo_d10d1_gc))


# Ratio Palma Ratio -------------------------------------------------------------
base_palma_gc <- 
  lmer(egal~homclass_gc+know_total_gc+class3+female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         palmaratio +loggdppercapita + rel_red +
         (1|country2),data=dfreg,weights = WEIGHT)

int_homo_palma_gc <- 
  update(base_palma_gc, . ~ . 
         +homclass_gc*class3*palmaratio -(1|country2) +
           (homclass_gc+class3|country2))

knitreg(list(base_palma_gc,int_homo_palma_gc))


# Share s80s20 Ratio------------------------------------------------------------
base_s80s20_gc <- 
  lmer(egal~homclass_gc+know_total_gc+class3+female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         s80s20 +loggdppercapita + rel_red +
         (1|country2),data=dfreg,weights = WEIGHT)

int_homo_s80s20_gc <- 
  update(base_s80s20_gc, . ~ . 
         +homclass_gc*class3*s80s20 -(1|country2) +
           (homclass_gc+class3|country2))

knitreg(list(base_s80s20_gc,int_homo_s80s20_gc))

# Share top10 -------------------------------------------------------------
base_top10_gc <- 
  lmer(egal~homclass_gc+know_total_gc+class3+female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         top10 +loggdppercapita + rel_red +
         (1|country2),data=dfreg,weights = WEIGHT)

int_homo_top10_gc <- 
  update(base_top10_gc, . ~ . 
         +homclass_gc*class3*top10 -(1|country2) +
           (homclass_gc+class3|country2))

knitreg(list(base_top10_gc,int_homo_top10_gc))


# Relative redistribution-------------------------------------------------------
base_relred_gc <- 
  lmer(egal~homclass_gc+know_total_gc+class3+female_gc+agenum_gc+age2_gc+
         edyears_gc+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+
         union_gc+workst_gc+
         rel_red +loggdppercapita + 
         (1|country2),data=dfreg,weights = WEIGHT)

int_homo_relred_gc <- 
  update(base_relred_gc, . ~ . 
         +homclass_gc*class3*rel_red -(1|country2) +
           (homclass_gc+class3|country2))

knitreg(list(base_relred_gc,int_homo_relred_gc))

interaction_terms <- c(
  "homclass_gc",
  "class3Intermediate class (III+IV+V)",
  "class3Working Class (VI+VII)",
  "loggdppercapita",
  "rel_red",
  "homclass_gc:class3Intermediate class (III+IV+V)",
  "homclass_gc:class3Working Class (VI+VII)",
  "gini_disp",
  "homclass_gc:gini_disp",
  "class3Intermediate class (III+IV+V):gini_disp",
  "class3Working Class (VI+VII):gini_disp",
  "homclass_gc:class3Intermediate class (III+IV+V):gini_disp",
  "homclass_gc:class3Working Class (VI+VII):gini_disp",
  "d10d1",
  "homclass_gc:d10d1",
  "class3Intermediate class (III+IV+V):d10d1",
  "class3Working Class (VI+VII):d10d1",
  "homclass_gc:class3Intermediate class (III+IV+V):d10d1",
  "homclass_gc:class3Working Class (VI+VII):d10d1",
  "palmaratio",
  "homclass_gc:palmaratio",
  "class3Intermediate class (III+IV+V):palmaratio",
  "class3Working Class (VI+VII):palmaratio",
  "homclass_gc:class3Intermediate class (III+IV+V):palmaratio",
  "homclass_gc:class3Working Class (VI+VII):palmaratio",
  "s80s20",
  "homclass_gc:s80s20",
  "class3Intermediate class (III+IV+V):s80s20",
  "class3Working Class (VI+VII):s80s20",
  "homclass_gc:class3Intermediate class (III+IV+V):s80s20",
  "homclass_gc:class3Working Class (VI+VII):s80s20",
  "top10",
  "homclass_gc:top10",
  "class3Intermediate class (III+IV+V):top10",
  "class3Working Class (VI+VII):top10",
  "homclass_gc:class3Intermediate class (III+IV+V):top10",
  "homclass_gc:class3Working Class (VI+VII):top10"
)
# Convert the character vector to a named list
interaction_list <- as.list(setNames(interaction_terms, interaction_terms))

# texreg::screenreg(list(int_homo_giniD_gc,
#                       int_homo_d10d1_gc,
#                       int_homo_palma_gc,
#                       int_homo_s80s20_gc,
#                       int_homo_top10_gc),single.row = T,
#                   custom.coef.map = interaction_list,
#                   file = "output/tables/int_homo_ineq_full-sample.txt")


texreg::plotreg(list(int_homo_giniD_gc,
                      int_homo_d10d1_gc,
                      int_homo_palma_gc,
                      int_homo_s80s20_gc,
                      int_homo_top10_gc),single.row = T,
                  custom.coef.map = interaction_list) +

# texreg::screenreg(list(int_homo_giniD_gc,
#                      int_homo_d10d1_gc,
#                      int_homo_palma_gc,
#                      int_homo_s80s20_gc,
#                      int_homo_top10_gc
#                      ),single.row = T,
#                   custom.coef.map = interaction_list,
#                   file = "output/tables/int_homo_ineq_oecd.txt")

# CROSS-LEVEL INTERACTION WITH CENTERED VARIABLES ------------------------------
############################################################################-


### Predicted values homo*class*GiniM ---------------------------------------
ginim_max<- round(mean(dfreg$gini_mkt) + sd(dfreg$gini_mkt)  ,2)
ginim_min<- round(mean(dfreg$gini_mkt) -sd(dfreg$gini_mkt) ,2)
ginim_mea<- round(mean(dfreg$gini_mkt),2)

df_pred_giniM_gc <-
  predictions(
    int_homo_giniM_gc,newdata = datagrid(
      gini_mkt = c(ginim_min, ginim_mea, ginim_max),
      homclass_gc = seq(min(dfreg$homclass_gc), max(dfreg$homclass_gc), by= 0.1),
      class3=levels(dfreg$class3)
    )
  )

df_pred_giniM_gc <- as.data.frame(df_pred_giniM_gc)
df_pred_giniM_gc$gini_mkt <- factor(df_pred_giniM_gc$gini_mkt,labels = c("Gini (-1SD)","Gini (Mean)", "Gini (+1SD)"))

df_pred_giniM_gc %>% 
  ggplot(aes(y=estimate,x=homclass_gc, fill=class3,color=class3,ymin=conf.low, ymax=conf.high)) +
  geom_line(size=0.75) +
  geom_point(size=2) +
  geom_ribbon(alpha=0.1,size=1,width=.1,linetype=1) +
  facet_wrap(~gini_mkt) +
  labs(y = "Preferences for Redistribution",
       x="Class-based network homogeneity (CWC)",
       title = "Three-way interaccion effects for Preferences for Redistribution, Network segregation, Social class and Income Inequality") +
  # scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Income inequality (Market)", breaks = NULL, labels = NULL)) +
  scale_fill_manual(values=c("#377EB8","#4DAF4A","#E41A1C"))+
  scale_color_manual(values=c("#377EB8","#4DAF4A","#E41A1C"))+  
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        axis.text=element_text(size=15),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

ggsave(plot = last_plot(),filename = "predict-egal-hom_class_giniM_fullsample_CWC.png",device = "png",
       path = here::here("output/images"),width = 2,height = 1,units = "cm",scale = 18)

### Predicted values homo*class*GiniD ---------------------------------------
# ginim_max<- round(max(dfreg$gini_mkt) ,2)
# ginim_min<- round(min(dfreg$gini_mkt),2)

ginid_max<- round(mean(dfreg$gini_disp) + sd(dfreg$gini_disp)  ,2)
ginid_min<- round(mean(dfreg$gini_disp) -sd(dfreg$gini_disp) ,2)
ginid_mea<- round(mean(dfreg$gini_disp),2)

df_pred_giniD_gc <-
  predictions(
    int_homo_giniD_gc,newdata = datagrid(
      gini_disp = c(ginid_min, ginid_mea, ginid_max),
      homclass_gc = seq(min(dfreg$homclass_gc), max(dfreg$homclass_gc), by= 0.1),
      class3=levels(dfreg$class3)
    )
  )

df_pred_giniD_gc <- as.data.frame(df_pred_giniD_gc)
df_pred_giniD_gc$gini_disp <- factor(df_pred_giniD_gc$gini_disp,labels = c("Gini (-1SD)","Gini (Mean)", "Gini (+1SD)"))


df_pred_giniD_gc %>% 
  ggplot(aes(y=estimate,x=homclass_gc, fill=class3,color=class3,ymin=conf.low, ymax=conf.high)) +
  geom_line(size=0.75) +
  geom_point(size=2) +
  geom_ribbon(alpha=0.1,size=1,linetype=1) +
  facet_wrap(~gini_disp) +
  labs(y = "Preferences for Redistribution",
       x="Class-based network homogeneity (CWC)",
       title = "Three-way interaccion effects for Preferences for Redistribution, Network segregation, Social class and Income Inequality") +
  # scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Income inequality (Disposable)", breaks = NULL, labels = NULL)) +
  scale_fill_manual(values=c("#377EB8","#4DAF4A","#E41A1C"))+
  scale_color_manual(values=c("#377EB8","#4DAF4A","#E41A1C"))+
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        axis.text=element_text(size=15),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

ggsave(plot = last_plot(),filename = "predict-egal-hom_class_giniD_fullsample_CWC.png",device = "png",
       path = here::here("output/images"),width = 2,height = 1,units = "cm",scale = 18)


df_pred_giniD_gc_1 <- df_pred_giniD_gc %>% filter(class3!="Intermediate class (III+IV+V)")
df_pred_giniD_gc_1 %>% 
  ggplot(aes(y=estimate,x=homclass_gc, fill=class3,color=class3,ymin=conf.low, ymax=conf.high)) +
  geom_line(size=0.75) +
  geom_point(size=2) +
  geom_ribbon(alpha=0.1,size=1,linetype=1) +
  facet_wrap(~gini_disp) +
  labs(y = "Preferences for Redistribution",
       x="Class-based network homogeneity (CWC)",
       title = "Three-way interaccion effects for Preferences for Redistribution, Network segregation, Social class and Income Inequality") +
  # scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Income inequality (Disposable)", breaks = NULL, labels = NULL)) +
  scale_fill_manual(values=c("#377EB8","#E41A1C"))+
  scale_color_manual(values=c("#377EB8","#E41A1C"))+
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        axis.text=element_text(size=15),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

ggsave(plot = last_plot(),filename = "predict-egal-hom_class_giniD_fullsample_CWC_2.png",device = "png",
       path = here::here("output/images"),width = 2,height = 1,units = "cm",scale = 18)



### Predicted values homo*class*top10 ---------------------------------------
top10_max<- round(mean(dfreg$top10) + sd(dfreg$top10)  ,2)
top10_min<- round(mean(dfreg$top10) -sd(dfreg$top10) ,2)
top10_mea<- round(mean(dfreg$top10),2)

df_pred_top10_gc <-
  predictions(
    int_homo_top10_gc,newdata = datagrid(
      top10 = c(top10_min, top10_mea, top10_max),
      homclass_gc = seq(min(dfreg$homclass_gc), max(dfreg$homclass_gc), by= 0.1),
      class3=levels(dfreg$class3)
    )
  )

df_pred_top10_gc <- as.data.frame(df_pred_top10_gc)
df_pred_top10_gc$top10 <- factor(df_pred_top10_gc$top10,labels = c("Top 10% (-1SD)","Top 10% (Mean)", "Top 10% (+1SD)"))

df_pred_top10_gc %>% 
  ggplot(aes(y=estimate,x=homclass_gc, fill=class3,color=class3,ymin=conf.low, ymax=conf.high)) +
  geom_line(size=0.75) +
  geom_point(size=2) +
  geom_ribbon(alpha=0.1,size=1,width=.1,linetype=1) +
  facet_wrap(~top10) +
  labs(y = "Preferences for Redistribution",
       x="Class-based network homogeneity (CWC)",
       title = "Three-way interaccion effects for Preferences for Redistribution, Network segregation, Social class and Top 10% income Share") +
  # scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Top 10%", breaks = NULL, labels = NULL)) +
  scale_fill_manual(values=c("#377EB8","#4DAF4A","#E41A1C"))+
  scale_color_manual(values=c("#377EB8","#4DAF4A","#E41A1C"))+  
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        axis.text=element_text(size=15),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))


ggsave(plot = last_plot(),filename = "predict-egal-hom_class_top10_fullsample_CWC.png",device = "png",
       path = here::here("output/images"),width = 2,height = 1,units = "cm",scale = 18)


### Predicted values homo*class*rel_red ---------------------------------------
relred_max<- round(mean(dfreg$rel_red) + sd(dfreg$rel_red)  ,2)
relred_min<- round(mean(dfreg$rel_red) -sd(dfreg$rel_red) ,2)
relred_mea<- round(mean(dfreg$rel_red),2)

df_pred_relred_gc <-
  predictions(
    int_homo_relred_gc,newdata = datagrid(
      rel_red = c(relred_min, relred_mea, relred_max),
      homclass_gc = seq(min(dfreg$homclass_gc), max(dfreg$homclass_gc), by= 0.1),
      class3=levels(dfreg$class3)
    )
  )

df_pred_relred_gc <- as.data.frame(df_pred_relred_gc)
df_pred_relred_gc$rel_red <- factor(df_pred_relred_gc$rel_red,labels = c("Rel. Redis. (-1SD)","Rel. Redis. (Mean)", "Rel. Redis.(+1SD)"))

df_pred_relred_gc %>% 
  ggplot(aes(y=estimate,x=homclass_gc, fill=class3,color=class3,ymin=conf.low, ymax=conf.high)) +
  geom_line(size=0.75) +
  geom_point(size=2) +
  geom_ribbon(alpha=0.1,size=1,width=.1,linetype=1) +
  facet_wrap(~rel_red) +
  labs(y = "Preferences for Redistribution",
       x="Class-based network homogeneity (CWC)",
       title = "Three-way interaccion effects for Preferences for Redistribution, Network segregation, Social class and Relative Redistribution") +
  # scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Relative Redistribution", breaks = NULL, labels = NULL)) +
  scale_fill_manual(values=c("#377EB8","#4DAF4A","#E41A1C"))+
  scale_color_manual(values=c("#377EB8","#4DAF4A","#E41A1C"))+  
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        axis.text=element_text(size=15),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))


ggsave(plot = last_plot(),filename = "predict-egal-hom_class_relRed_fullsample_CWC.png",device = "png",
       path = here::here("output/images"),width = 2,height = 1,units = "cm",scale = 18)

