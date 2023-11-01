if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,sjmisc,ggplot2,interplot,marginaleffects,sjlabelled,haven,stringr,ordinal,ggplot2)  
rm(list=ls())
load(here::here("input/data/proc/study1_country.RData"));df1 <- df2
df1$egal <- df1$redist+df1$prefineq

# Porcentaje de varianza explicada
fit1<- psych::fa(df1[,c("redist","prefineq")],nfactors = 1,rotate = "varimax",missing = F,weight = df1$WEIGHT)
fit1$loadings
fs<- as.data.frame(fit1$scores)
names(fs) <- c("lvegal")
df1 <- bind_cols(df1,fs)

df1[,c("redist","prefineq","lvegal")] %>% sjPlot::tab_corr()
summary(df1$lvegal)

model1 <- 
'cfaegal=~ redist +prefineq ' 


fit1 <- lavaan::cfa(model1,data = df1,sampling.weights = "WEIGHT",cluster = "country2",missing = "ML")
summary(fit1)
predicted2 <- lavaan::lavPredict(fit1,newdata = df1)  

idx <- lavaan::lavInspect(fit1, "case.idx")
fscores <- lavaan::lavPredict(fit1)
## loop over factors
for (fs in colnames(fscores)) {
  df1[idx, fs] <- fscores[ , fs]
}

summary(df1$cfaegal)
summary(df1$egal2)
summary(df1$lvegal)

df1[,c("egal2","cfaegal","lvegal")] %>% cor(use = "na.or.complete")

dfreg <- df1 %>% dplyr::select(egal2=egal2,
                               redist,
                               prefineq,
                               class3_ser_gc,
                               class3_mid_gc,
                               class3_wor_gc,
                               # class3_ser_gc=class3res_ser_gc, 
                               # class3_mid_gc=class3res_mid_gc, 
                               # class3_wor_gc=class3res_wor_gc,
                               # class3spo,
                               homclass = homclass_gc,
                               Q03pcm_1_gc, Q03pcm_2_gc, Q03pcm_3_gc, Q03pcm_NA_gc,
                               edyears = edyears_gc,
                               female = female_gc,
                               agenum = agenum_gc,
                               union = union_gc,
                               workst = workst_gc,
                               #----------------------------
                                 # gini_disp=gini_disp_c,
                                 # gini_mkt=gini_mkt_c,
                                 # palmaratio=palmaratio_c,
                                 # top10=top10_c,
                                 # middle50=middle50_c,
                                 # d10d1=d10d1_c,
                                 # giniindex=giniindex_c,
                                 # ttheilge1=ttheilge1_c,
                                #----------------------------
                                 # gini_disp=gini_disp_z,
                                 # gini_mkt=gini_mkt_z,
                                 # palmaratio=palmaratio_z,
                                 # top10=top10_z,
                                 # middle50=middle50_z,
                                 # d10d1=d10d1_z,
                                 # giniindex=giniindex_z,
                                 # ttheilge1=ttheilge1_z,
                               #------------------------------
                               gini_disp,
                               gini_mkt,
                               palmaratio,
                               top10,
                               middle50,
                               d10d1,
                               giniindex,
                               ttheilge1,
                                 WEIGHT,
                                 region,                              
                                 country2,
                                 country,
                               rgdpna,
                               oecd
                                 ) %>% 
mutate(logrgdpna=log(rgdpna),
       edyears2=edyears^2,
       age2=agenum^2) %>%
  # mutate(homclass=group_center(homclass,country2))  %>%
  # filter(!country2 %in% c("TWN","CHN","JPN")) %>%
  # filter(oecd=="OECD" & country2!="SVN") %>% 
  filter(country2!="SVN") %>% 
  na.omit()


base <- lmer(egal2~1 + (1|country2),data=dfreg,weights = WEIGHT); icc1<- performance::icc(base)
homclass<- update(base, . ~ . +homclass)
full1 <- update(homclass, . ~ . +class3_mid_gc+class3_wor_gc +female+agenum+age2)
rob1 <- update(full1, . ~ . + edyears+ Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+union+workst)
rob2 <- update(rob1, . ~ . -(1|country2) + (homclass|country2))

# Interactions segregation x class
int_homo <- update(rob1, . ~ . +class3_mid_gc*homclass+class3_wor_gc*homclass)
models <- list(homclass,full1,rob1,int_homo)
texreg::knitreg(models,stars = c(.001, .01, .05, .1),symbol = '\u2020')

  # Homoclass x Other Economic Inequality-----------------------------------------
main <- "egal2~class3_mid_gc+class3_wor_gc+homclass+edyears+Q03pcm_2_gc+Q03pcm_3_gc+Q03pcm_NA_gc+workst+union+female+agenum+age2+(homclass|country2)"
base <- lmer(formula(main),data=dfreg,weights = WEIGHT)

int_ginid <- update(base, . ~ . +homclass*gini_disp)
int_ginim <- update(base, . ~ . +homclass*gini_mkt)
int_palma <- update(base, . ~ . +homclass*palmaratio)
int_palma <- update(base, . ~ . +homclass*palmaratio)
int_pc10  <- update(base, . ~ . +homclass*top10)
int_pc50  <- update(base, . ~ . +homclass*middle50)
int_d10d1 <- update(base, . ~ . +homclass*d10d1)
int_gini_wiid <- update(base, . ~ . +homclass*giniindex)
int_theil  <- update(base, . ~ . +homclass*ttheilge1)

models_m <- list(int_ginid,int_ginim,int_palma,int_pc10,int_pc50,int_d10d1,
                 int_gini_wiid,int_theil)

texreg::knitreg(models_m,stars = c(.001, .01, .05, .1))
interplot::interplot(m = int_ginim,var1 = "homclass",var2="gini_mkt",point = T)

predval1 <- 
  plot_predictions(int_ginim, condition = list("homclass",gini_mkt=range),gray = F) +
  xlab("Network homogeneity")+
  ylab("Redistributive preferences") +
  theme(legend.position = "right")


plot_df <- 
broom_df %>% 
  filter(term == "class3_mid_gc:homclass" | term =="homclass:class3_wor_gc") %>% 
  arrange(level) 
ggplot(
  plot_df,
  aes(x = estimate, y = level ,group=term, fill=term)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),size=1, width=0.2) +
  geom_point(shape=21, size=3, fill="white") +
  geom_vline(xintercept = 0) +
  scale_x_discrete() +
  facet_grid(~term)


if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,sjmisc,ggplot2,interplot,marginaleffects,haven,ordinal,brms)  
rm(list=ls())
load(here::here("input/data/proc/study1_country.RData"));df1 <- df2

df1$redistOR <- ordered(x = df1$redist)
dfreg <- df1 %>% dplyr::select(redistOR,
                               class3,
                               homclass=homclass_gc,
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
                               rgdpna,
                               oecd,
                               country2,country
) %>% 
  mutate(logrgdpna=log(rgdpna),
         edyears2=edyears^2,
         age2=agenum^2) %>%
  na.omit()

# Models 
base <- clm(redistOR~1 + factor(country2),data=dfreg,weights = WEIGHT)
homclass<- update(base, . ~ . +homclass)
full1 <- update(homclass, . ~ . + class3+female+agenum+age2)
rob1 <- update(full1, . ~ . + edyears+ Q03pcm+union+workst)

# Interactions segregation x class
int_homo <- update(rob1, . ~ . +class3*homclass)
models <- list(homclass,full1,rob1,int_homo)
texreg::screenreg(models,omit.coef = "(country2)")

# Create dummy dataset for predictions
newdata = datagrid(
  model = int_homo,
  redistOR = dfreg$redistOR,
  homclass = c(
    mean(dfreg$homclass) - sd(dfreg$homclass),
    mean(dfreg$homclass),
    mean(dfreg$homclass) + sd(dfreg$homclass)
  ),
  class3 = dfreg$class3
)

p <- predictions(model = int_homo,by = c("homclass","class3"),newdata = newdata)

p2 <- 
p %>% 
  filter(group %in% c(1,3,5) &
           class3 %in% c("Service Class (I+II)","Working Class (V+VI+VII)"))

p2$class3 <- factor(p2$class3,levels = c("Working Class (V+VI+VII)","Service Class (I+II)"))
p2$group  <- factor(p2$group,labels = c("Strongly disagree",
                                        "Disagree Neither \n agree nor disagree",
                                        "Strongly agree"))

p2 %>%
  ggplot(aes(
    x = class3,
    y = estimate,
    fill = factor(homclass)
  )) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width=.2,
    linewidth = 1,
    position = position_dodge(0.9)
  ) +
  facet_grid( ~ group) +
  labs(title = "",
       x = NULL,
       y = "Predicted probability",
       fill = "Homogeneity") +
  scale_fill_manual(labels = c("- 1 SD", "Mean", "+ 1 SD"),
                    values = RColorBrewer::brewer.pal(name = "Greys",n = 3)) +
  scale_x_discrete(labels=c("Working Class","Service Class")) +
  scale_y_continuous(n.breaks = 10) +
  theme_bw() +
  theme(legend.position = "bottom")
  


  
  
