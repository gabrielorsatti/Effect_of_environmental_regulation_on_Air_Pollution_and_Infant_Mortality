***********************************************************************************
* File: Table6_01102014.do
* Updated for Collaboration: Gab & Matis
***********************************************************************************

* 0. Setup
clear all
set more off
cap set mem 500m
cap set matsize 10000

* Fermeture de tous les logs ouverts
cap log close _all

* ------------------------------------------------------------------------------
* GESTION DES CHEMINS AUTOMATIQUE
* ------------------------------------------------------------------------------
if "`c(username)'" == "gab" {
    global mypath "C:/Users/gab/Documents/GitHub/ Effect_of_environmental_regulation_on_Air_Pollution_and_Infant_Mortality/"
}
else {
    * comme dh'ab ty dois ajuster son nom d'utilisateur ici
    global mypath "C:/Users/tonom/Documents/GitHub/Effect_of_environmental_regulation_on_Air_Pollution_and_Infant_Mortality/"
}

* Définition du dossier de travail
cd "$mypath"

* Création automatique des dossiers de sortie (pour éviter les erreurs d'écriture)
cap mkdir "Log"
cap mkdir "Output"
cap mkdir "Output/T6"

* Lancement du log
log using "Log/Table6_01102014.txt", replace text name(logT6)

***********************************************************************************
* 1. Data prep: Generate policy event-years
***********************************************************************************

* Utilisation du chemin relatif à partir de la racine du GitHub
u "data/im_air.dta", clear

egen IMtemp = max(c_IM), by(state city)
drop if IMtemp >= .
drop IMtemp
egen city_id = group(city)
bys state city: egen count = count(c_IM)

egen temp3 = min(year), by(city_id)
egen temp35 = max(year), by(city_id) 
g tau = year - catyear if catyear > temp3 & c_IM<.
g neveradoptCAT = (catyear >= . | temp35 < catyear)
g tempx = 1 if year>=catyear & catyear<. & c_IM<.
egen tempy = min(tempx) if catyear<., by(city_id)
replace neveradoptCAT = 1 if tempy>=. & catyear<.		
replace tau = . if tempy>=. & catyear<.

g temp4 = tau >= 3 & tau < .
g temp5 = tau <= -3 & tau < .
egen Mtemp4 = max(temp4), by(city_id) 
egen Mtemp5 = max(temp5), by(city_id)
g useCAT = (Mtemp4 == 1 & Mtemp5 == 1 & count > 1) | (neveradoptCAT == 1 & count > 1)

drop if useCAT==0
drop if c_IM >= .
g one = 1

forv tau = 10(-1)1 {
	g taum`tau' = tau == -`tau'
	la var taum`tau' "This obs is `tau' years before catalytic converters made mandatory"
}
forv tau = 0/5 {
	g tau`tau' = tau == `tau'
	la var tau`tau' "This obs is `tau' years after catalytic converters made mandatory"
}
g tauL = tau<-10
g tauR = tau>5 & tau<.

***********************************************************************************
* 2. Run Two-stage Regression Analysis
***********************************************************************************

xi: reg c_IM taum10-taum1 tau0-tau5 tauL tauR lit_urban mean i.city i.year [aw = c_birth]	
g taub = .
g tause = .

* Le collapse va réduire le dataset en mémoire
collapse (mean) taub tause taum10-tau5, by(tau)
drop if tau>5 | tau<-10
foreach k of varlist taum10-taum1 tau0-tau5 {			
	replace taub = _b[`k'] if `k' == 1
	replace tause = _se[`k'] if `k' == 1
}
drop taum10-tau5
g catconv = (tau>=0)
g tau_trend = tau + 11
g catconv_trend = catconv*tau
replace catconv_trend = catconv_trend + 1 if tau>=0

/* Regressions de la deuxième étape */
* Eqn (2a)
reg taub catconv [aw=1/tause]
	outreg2 catconv using "Output/T6/Table6_CConIM.xls", dec(2) se par ctitle("Eqn 2a - IM") aster(coef) replace	

* Eqn (2b)	
reg taub catconv tau [aw=1/tause]
	outreg2 catconv tau using "Output/T6/Table6_CConIM.xls", dec(2) se par ctitle("Eqn 2b - IM") aster(coef) append

* Eqn (2c)	
reg taub catconv tau catconv_trend [aw=1/tause]
	lincom catconv + 5*catconv_trend
	local coef = r(estimate)
	local coef = round(`coef', .01)
	local coef = "`coef'"
	if length("`coef'") < 3 {
		local coef = "`coef'" + "0"
	}
	local coef = substr("`coef'", 1, 6)
	local tstat = r(estimate)/r(se)
	local pval = tprob(r(df), abs(`tstat'))
	local pval = round(`pval', .01)
    
	* Système d'étoiles pour la significativité
	if `pval' <= .01 {
		local coef = "`coef'" + "***"
	}
	else if `pval' <= .05 {
		local coef = "`coef'" + "**"
	}
	else if `pval' <= .1 {
		local coef = "`coef'" + "*"
	}
    
	if length("`pval'") < 3 {
		local pval = "`pval'" + "0"
	}
	outreg2 catconv tau catconv_trend using "Output/T6/Table6_CConIM.xls", dec(2) se par ctitle("Eqn 2c - IM") aster(coef) addtext("5-Year Effect", "`coef'", "p-value", "[`pval']") append		

***********************************************************************************
* 3. Clean-up
***********************************************************************************
cap log close logT6