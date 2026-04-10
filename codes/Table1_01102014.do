***********************************************************************************
* File: Table1_01102014.do
* Authors: Rema Hanna, Michael Greenstone
* Modified for Collaboration
***********************************************************************************

clear all
set more off
cap log close _all

* ------------------------------------------------------------------------------
* GESTION DES CHEMINS AUTOMATIQUE
* ------------------------------------------------------------------------------
if "`c(username)'" == "gab" {
    global mypath "C:/Users/gab/Documents/GitHub/ Effect_of_environmental_regulation_on_Air_Pollution_and_Infant_Mortality/"
}
else {
    * Ici, Matis tu dois indiquer ton propre chemin une seule fois
    global mypath "C:/Users/NOM/Documents/GitHub/Effect_of_environmental_regulation_on_Air_Pollution_and_Infant_Mortality/"
}

* On définit le dossier de travail (Working Directory)
cd "$mypath"

* Ouverture du Log en utilisant le global
cap mkdir "Log" // Crée le dossier Log s'il n'existe pas
log using "${mypath}Log/Table1_01102014.txt", replace text name(logT1_tab)

************************
/* Air Pollution */
* Utilisation du global pour charger les données
u "${mypath}data/combined.dta", clear


egen city_id = group(city)
bys state city: egen count = count(e_spm_mean)

cap drop temp*
g temp = year if actionplan_sc == 1
egen temp2 = min(temp), by(city_id) 
egen temp3 = min(year), by(city_id) 
g tauSC = year - temp2 if temp2 > temp3 & (e_spm_mean<. | e_so2_mean<. | e_no2_mean<.)
g neveradoptSC = temp2 >= .
g tempx = 1 if actionplan_sc==1 & (e_spm_mean<. | e_so2_mean<. | e_no2_mean<.)		// the following 3 lines of code determine whether there are any cities which enacted the policy but are missing pollution data for ALL post-policy years
egen tempy = min(tempx) if temp2<., by(city_id)
ta tempy if temp2<., missing		// no cities fit this description, so no more changes need be made

g temp4 = tauSC >= 3 & tauSC < .
g temp5 = tauSC <= -3 & tauSC < .
egen Mtemp4 = max(temp4), by(city_id)
egen Mtemp5 = max(temp5), by(city_id)
g useSC = (Mtemp4 == 1 & Mtemp5 == 1 & count > 1) | (neveradoptSC == 1 & count > 1)

drop Mtemp* temp*
g temp = year if catconverter == 1
egen temp2 = min(temp), by(city_id)
egen temp3 = min(year), by(city_id)
g tauCAT = year - temp2 if temp2 > temp3 & (e_spm_mean<. | e_so2_mean<. | e_no2_mean<.)
g neveradoptCAT = temp2 >= .
g tempx = 1 if catconverter==1 & (e_spm_mean<. | e_so2_mean<. | e_no2_mean<.)		// the following 3 lines of code determine whether there are any cities which enacted the policy but are missing pollution data for ALL post-policy years
egen tempy = min(tempx) if temp2<., by(city_id)
ta tempy if temp2<., missing		// Silvassa enacted catconv in 1998 but has missing poll data starting in 1996. Include it.
replace neveradoptCAT = 1 if tempy>=. & temp2<.
replace tauCAT = . if tempy>=. & temp2<.

g temp4 = tauCAT >= 3 & tauCAT < .
g temp5 = tauCAT <= -3 & tauCAT < .
egen Mtemp4 = max(temp4), by(city_id) 
egen Mtemp5 = max(temp5), by(city_id)
g useCAT = (Mtemp4 == 1 & Mtemp5 == 1 & count > 1) | (neveradoptCAT == 1 & count > 1)

drop if useSC==0 | useCAT==0
drop if e_spm_mean >= .

log close logT1_tab

******Air Pollution**********

log using "${mypath}Output/T1/T1_tab.txt", replace text name(T1_tab)

* All Included Obs *
ta year
* Only Obs with Policy in Place *
ta year if actionplan_sc==1
ta year if catconv==1

log close T1_tab

*****************
* Water Pollution

u "${mypath}data/india_waters_cityyear.dta", clear
cap egen cityriver = group(city river)
foreach var of varlist month_* {
	replace `var' = 0 if year == 2005
}
cap drop temp*
egen yap1city = max(yap1), by(cityriver)
egen gap1city = max(gap1), by(cityriver)
egen gap2city = max(gap2), by(cityriver)
egen nrcpcity = max(nrcp), by(cityriver)
egen dapcity  = max(dap),  by(cityriver)
egen gomtiap1city = max(gomtiap1), by(cityriver)
g temp2 = 1995 if nrcpcity==1
replace temp2 = 1993 if yap1city==1 | gap2city==1 | dapcity==1 | gomtiap1city==1
replace temp2 = 1985 if gap1city==1
egen temp3 = min(year), by(cityriver) 
g tau = year - temp2 if temp2 > temp3 & (bod<. | do<. | lnfcoli<.)
g neveradopt = temp2 >= .
g tempx = 1 if (yap1==1 | gap1==1 | gap2==1 | nrcp==1 | dap==1 | gomtiap1==1) & (bod<. | lnfcoli<. | do<.)		// the following 3 lines of code determine whether there are any cities which enacted the policy but are missing pollution data for ALL post-policy years
egen tempy = min(tempx) if temp2<., by(cityriver)
ta tempy if temp2<., missing		// 7, but these cities have no non-missing data of ANY kind so they should not be added back in.

g temp4 = tau >= 3 & tau < .
g temp5 = tau <= -3 & tau < .
egen Mtemp4 = max(temp4), by(cityriver) 
egen Mtemp5 = max(temp5), by(cityriver) 
g use = (Mtemp4 == 1 & Mtemp5 == 1) | neveradopt == 1

keep if use==1
drop if bod>=. & do>=. & lnfcoli>=.
bys state city: g count = _n
bys state city : egen cityno = max(count)
drop if cityno<2

******Water Pollution**********

log using "${mypath}Output/T1/T1_tab.txt", append text name(T1_tab)

* All Included Obs *
ta year
* Only Obs with Policy in Place *
ta year if yap1==1 | gap1==1 | gap2==1 | dap==1 | gomtiap1==1 | nrcp==1

log close T1_tab
