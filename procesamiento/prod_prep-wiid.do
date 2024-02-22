*ssc install wid
help wid


wid, indicators(gdiinc) areas(AU AT CN TW HR CZ DK EE FI FR DE HU IS IN IL JP LT MX NZ PH RU SK SI ZA ES SR SE CH TH TR GB US) year(2016 2017 2018) perc(_all) ages(992) pop(j) clear

drop variable
reshape wide value, i(country year) j(percentile) string
rename valuep0p100 gini_ptax
