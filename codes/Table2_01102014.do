***********************************************************************************
* File: Table2_01102014.do
* Modified by: Jon Petkun (jbpetkun@mit.edu)
* Updated for Collaboration: Gab & Matis
***********************************************************************************

clear all
set more off
cap log close _all  // Ferme tous les logs pour éviter l'erreur r(604)

* ------------------------------------------------------------------------------
* 1. GESTION DES CHEMINS AUTOMATIQUE
* ------------------------------------------------------------------------------
if "`c(username)'" == "gab" {
    global mypath "C:/Users/gab/Documents/GitHub/ Effect_of_environmental_regulation_on_Air_Pollution_and_Infant_Mortality/"
}
else {
    * Remplacer "MATIS" par le vrai nom d'utilisateur Windows de Matis
    global mypath "C:/Users/NOM_a_mettre/Documents/GitHub/Effect_of_environmental_regulation_on_Air_Pollution_and_Infant_Mortality/"
}

* Définition du dossier de travail
cd "$mypath"

* Création automatique des dossiers si nécessaire (évite les erreurs d'exportation)
cap mkdir "Log"
cap mkdir "Output"
cap mkdir "Output/T2"

* Lancement du log
log using "Log/Table2_01102014.txt", replace text name(logT2)

***********************************************************************************
* Table 2. Summary Statistics (Long Difference)
***********************************************************************************

/* Air */
u "data/Combined.dta", clear

/* Keep only obs with some electronic air pollution info */
keep if e_spm_mean < . | e_spm_max < . | e_spm_min < . | e_so2_mean < . | e_so2_max < . | e_so2_min < . | e_no2_mean < . | e_no2_max < . | e_no2_min < . 

/* Collapse and export Full Period stats */
preserve
	foreach p in "spm" "so2" "no2" {
		g e_`p'_sd = e_`p'_mean
		g e_`p'_p10 = e_`p'_mean
		g e_`p'_p90 = e_`p'_mean
		g ones_`p' = 1 if e_`p'_mean < .
	}
	collapse (mean) *_mean (sd) *sd (sum) ones* (p10) *p10 (p90) *p90 
	foreach var of varlist e_spm_mean-ones_no2 {
		replace `var' = round(`var', .01)
	}
	outsheet using "Output/T2/T2_fp_air.txt", replace
restore

/* Keep early and late periods */
keep if year < 1991 | year > 2003
g ld = year > 2003

foreach p in "spm" "so2" "no2" {
	g ld_`p'_mean = e_`p'_mean
	g ld_`p'_sd = e_`p'_mean
	g ld_`p'_ones = 1 if e_`p'_mean < .
	g ld_`p'_p10 = e_`p'_mean
	g ld_`p'_p90 = e_`p'_mean
}

/* Collapse into long difference stats */
collapse (mean) ld_*_mean (sd) *sd (sum) *ones (p10) ld_*_p10 (p90) ld_*_p90, by(ld)
order ld ld_spm* ld_so2* ld_no2*

/* Rounding */
foreach var of varlist ld_* {
	replace `var' = round(`var', .01)
}
/* Export LD */
outsheet using "Output/T2/T2_ld_air.txt", replace

/* Water */
u "data/india_waters_cityyear.dta", clear

keep if bod < . | do < . | lnfcoli < .

/* Full period collapse and export */
preserve
	foreach p in "bod" "lnfcoli" "do" {
		g `p'_sd = `p'
		g `p'_p10 = `p'
		g `p'_p90 = `p'
		g ones_`p' = 1 if `p' < .
	}
	collapse (mean) bod lnfcoli do (sd) *sd (sum) ones* (p10) *p10 (p90) *p90 
	foreach var of varlist bod-ones_do {
		replace `var' = round(`var', .01)
	}
	outsheet using "Output/T2/T2_fp_water.txt", replace
restore

/* Keep early and late periods */
keep if year < 1990 | year > 2001
g ld = year > 2001

foreach p in "bod" "do" "lnfcoli" {
	g ld_`p'_mean = `p'
	g ld_`p'_sd = `p'
	g ld_`p'_p10 = `p'
	g ld_`p'_p90 = `p'
	g ld_`p'_ones = 1 if `p' < .
}
/* Collapse into long difference stats */
collapse (mean) ld*mean (sd) ld*sd (sum) ld*ones (p10) ld*p10 (p90) ld*p90, by (ld)
order ld ld_bod* ld_lnfcoli* ld_do*

foreach var of varlist ld_bod_mean-ld_do_p90 {
	replace `var' = round(`var', .01)
}
outsheet using "Output/T2/T2_ld_water.txt", replace

/* Infant Mortality */
u "data/im_air.dta", clear
joinby state city year using "data/im_water.dta", unmatched(both)

keep if c_IM<.

/* Full period collapse and export */
preserve
	g im_sd = c_IM
	g im_p10 = c_IM
	g im_p90 = c_IM
	g ones_im = 1 if c_IM < .
	
	collapse (mean) c_IM (sd) im_sd (sum) ones_im (p10) im_p10 (p90) im_p90 
	foreach var of varlist c_IM im_sd im_p10 im_p90 ones_im {
		replace `var' = round(`var', .01)
	}
	outsheet using "Output/T2/T2_fp_IM.txt", replace
restore

/* Keep early and late periods */
keep if year < 1991 | year > 2000
g ld = year > 2000

g ld_im_mean = c_IM
g ld_im_sd = c_IM
g ld_im_p10 = c_IM
g ld_im_p90 = c_IM
g ld_im_ones = 1 if c_IM < .

/* Collapse into long difference stats */
collapse (mean) ld*mean (sd) ld*sd (sum) ld*ones (p10) ld*p10 (p90) ld*p90 , by (ld)

foreach var of varlist ld_im_mean-ld_im_ones {
	replace `var' = round(`var', .01)
}
outsheet using "Output/T2/T2_ld_IM.txt", replace

cap log close logT2