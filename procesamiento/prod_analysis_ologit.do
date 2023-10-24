use "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\input\data\proc\study1_country.dta"
*For the regression tables:
*ssc install estout, replace
*https://repec.sowi.unibe.ch/stata/estout/esttab.html

gen logrgdpna = log(rgdpna)

*# Ordinal logit full sample ---------------------------------------------------
meglm redist c.homclass##c.gini_disp i.class3 female agenum edyears i.Q03pcm workst union logrgdpna gini_disp|| country2: homclass, family(ordinal)
estimates store model1

meglm redist c.homclass##c.gini_mkt i.class3 female agenum edyears i.Q03pcm workst union logrgdpna gini_mkt|| country2: homclass, family(ordinal)
estimates store model2

meglm redist c.homclass##c.top10 i.class3 female agenum edyears i.Q03pcm workst union logrgdpna top10|| country2: homclass, family(ordinal)
estimates store model3

meglm redist c.homclass##c.middle50 i.class3 female agenum edyears i.Q03pcm workst union logrgdpna middle50|| country2: homclass, family(ordinal)
estimates store model4

meglm redist c.homclass##c.d10d1 i.class3 female agenum edyears i.Q03pcm workst union logrgdpna d10d1|| country2: homclass, family(ordinal)
estimates store model5

meglm redist c.homclass##c.giniindex i.class3 female agenum edyears i.Q03pcm workst union logrgdpna giniindex|| country2: homclass, family(ordinal)
estimates store model6

meglm redist c.homclass##c.ttheilge1 i.class3 female agenum edyears i.Q03pcm workst union logrgdpna ttheilge1|| country2: homclass, family(ordinal)
estimates store model7

estimates save fullsample-ologit-homclass-respondent-macro
esttab model1 model2 model3 model4 model5 model6 model7, drop(logrgdpna 1.class3 2.class3 3.class3 female agenum edyears workst union 1.Q03pcm 2.Q03pcm 3.Q03pcm 4.Q03pcm )


*# Ordinal logit weak ties -----------------------------------------------------

*# Ordinal logit strong ties ---------------------------------------------------

*# Ordinal logit R's class-- ---------------------------------------------------
