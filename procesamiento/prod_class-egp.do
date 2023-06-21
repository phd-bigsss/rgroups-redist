clear
use "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\input\data\proc\ocup_respondent-spouse.dta" 
 
*ssc install isko
*help isko


save "P:\papers\ess-merit\input\data\proc\ELSOC_egp-isei-siops.dta", replace

*iskolab

*isko specifies the variable to be recoded.  This must be a 4 digit integer containing ISCO-88 occupational codes.

*sempl specifies a variable indicating whether or not the respondent is self-employed. A 1 indicates self-employment, all other values are ignored.

*supvis specifies a variable indicating the number of employees the respondent supervises. The values 1 and 10 are significant for placement in certain EGP categories.

*ssc install iscogen
*help iscogen
*codebook isco88
*recode nempleados (.=0)

codebook isco88r
codebook semp
codebook supvis

iskoegp class10 , isko(isco88r) sempl(semp) supvis(supvis)
tabulate class10
codebook class10


iskoegp class10spo , isko(isco88spo) sempl(sempspo) supvis(supvisspo)
tabulate class10spo
codebook class10spo

drop isco88r-supvisspo
save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\input\data\proc\class_respondent-spouse.dta", replace

