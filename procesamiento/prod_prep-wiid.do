*ssc install wid
*help wid


* diinc	(=) post-tax national income	post-tax national income
* 992 		The population is comprised of individuals over age 20.
* j			equal-split adults

wid, indicators(gdiinc) areas(AU AT CN TW HR CZ DK EE FI FR DE HU IS IN IL JP LT MX NZ PH RU SK SI ZA ES SR SE CH TH TR GB US) year(2016 2017 2018) perc(_all) ages(992) pop(j) clear
drop variable
reshape wide value, i(country year) j(percentile) string
rename valuep0p100 wid_gini_disp
label variable wid_gini_disp "Gini Disposable (WID)"
drop age pop
egen country_yr = concat(country year), punct(_)  // I separated them by a "-", you can change to whatever you want

save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\input\data\original\wid_gini.dta", replace
*These correspond to the income or wealth of the group, divided by the total for the whole population. For example, the income of the group p99p100 is the top 1% income share.

wid, indicators(adiinc) areas(AU AT CN TW HR CZ DK EE FI FR DE HU IS IN IL JP LT MX NZ PH RU SK SI ZA ES SR SE CH TH TR GB US) year(2015 2016 2017 2018) perc(p0p10 p0p50 p80p100 p90p100) ages(992) pop(j) clear 
*wid, indicators(acainc) areas(AU AT CN TW HR CZ DK EE FI FR DE HU IS IN IL JP LT MX NZ PH RU SK SI ZA ES SR SE CH TH TR GB US) year(2016 2017 2018) perc(p10p100 p20p100 p40p100 p50p100 p80p100 p90p100) ages(992) pop(j) clear 

reshape wide value, i(country year) j(percentile) string
egen country_yr = concat(country year), punct(_)  // I separated them by a "-", you can change to whatever you want
bro
gen wid_rd10d01 = valuep90p100/valuep0p10
label variable wid_rd10d01 "Ratio Top 10/Bottom 10"
gen wid_rp90p50 = valuep90p100/valuep0p50
label variable wid_rp90p50 "Ratio Top 10/Bottom 50"
bro
save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\input\data\original\wid_ratios.dta", replace

wid, indicators(sdiinc) areas(AU AT CN TW HR CZ DK EE FI FR DE HU IS IN IL JP LT MX NZ PH RU SK SI ZA ES SR SE CH TH TR GB US) year(2016 2017 2018) perc(p0p10 p80p100 p90p100) ages(992) pop(j) clear 
reshape wide value, i(country year) j(percentile) string
drop variable age pop
egen country_yr = concat(country year), punct(_)  // I separated them by a "-", you can change to whatever you want
rename valuep90p100 wid_sharetop10
label variable wid_sharetop10  "Top 10% share (WID)"
bro

save "C:\Users\jiturra\Documents\documentos-pcloud\rgroups-redist\input\data\original\wid_shares.dta", replace

