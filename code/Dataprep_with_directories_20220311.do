
version 15.0	
  
*** SETTINGS ***
clear all  		
set more off  	

*** DIRECTORY PATHS *** 	

* PROJECT local root directory
global d_proj = "C:\Users\Hossmann\Dropbox\Affiliation"
	
* BASIC folders
  * directory for data 		 
global d_data 	=  "$d_proj/data/"
  * directory for original data 		 
global d_origidata 	=  "$d_proj/source/"
  * directory for do-files	 
global d_do 	=  "$d_proj/script/"
  * directory for log-files		
global d_log 	= "$d_proj/log/"
  * directory for excel output 		 
global d_tab 	= "$d_proj/tables/"


****** Import and merge Excel from Pubmed and number author positions

		clear
		import delimited "$d_proj/source/clean_dataset_affiliation_gender_20210519.csv"     			// Dataset from scopus
			*Carole: in this data set we have more PMIDs (N = 325) than in the original IeDEA publication list (N = 308). New export from May 2020.
		
		** Number position based on the order from the extraction
		gen no = _n
		foreach var in pmid{
			sort `var' no
			by `var': gen author_position = _n
		}
		
		save "$d_proj/data/position_1.dta", replace		
		
			** Generate position working dataset from Pubmed
			*Carole: What do we do with that step? We extract the order of the authorlist from web of science and bring our file into the same order. Because the extraction from scopus made a mess.
			clear
			import delimited "$d_proj/data/csv-AI069924-set.csv", clear   // file exported from pubmed
			split authors, parse(,)
			drop authors
			reshape long authors, i(ïpmid) j(j)
			drop if authors =="" 
			split authors
			keep ïpmid j authors authors1 publicationyear
			rename ïpmid pmid
			rename j author_position
			save "$d_proj/data/position_working_dataset_pubmed.dta", replace
			*Carole: here we have N = 359 PMIDs. Where does the "csv-AI069924-set.csv" data set originate from? It comes from pubmed.
	
		** Merge pubmed dataset to identify collaborators and IeDEA etc. 
		
		clear
		use "$d_proj/data/position_1.dta"
		merge m:m pmid author_position using "$d_proj/data/position_working_dataset_pubmed.dta", force
			
			** Remove what was added by the export from Pubmed 
			drop if _merge == 2 & pmid != 19531928
		
			** Manually drop collaborators etc. !! Have a look at which were dropped..
			
			drop if pmid ==27578823 & author_position > 27			// Identified list of collaborators by the merge above  // 358
			replace surname = authors1 if pmid==30308013
			replace surname = authors1 if pmid==31332437
			drop if pmid ==29494593									// Collaboration -1
			drop if authors1 == "IeDEA" 							// -1 is onle one line
			drop if authors1 =="east"
			drop if authors1 =="and"
			drop if authors1 =="COHERE"
			drop if authors1 =="PHACS"
			drop if authors1 =="AIDS-defining"
			drop if authors1 =="Collaborative"
			drop if authors1 ==""
			drop if surname ==""
						
			drop authors authors1 _merge
			*Carole: N = 318 PMIDs
		
		** Generate 2 variables with 1 for last authors (last_position) and 1 for first author (first_position) respectively 
		egen last_position = max(author_position), by(pmid)
		replace last_position = 1 if author_position == last_position
		replace last_position = 0 if last_position != 1
		egen first_position = min(author_position), by(pmid)
		replace first_position = 0 if author_position != 1
		drop no
		
		
		save "$d_proj/data/position_2.dta", replace	
		
			** Merge with initial manual table
			clear
			import excel "$d_proj/source\SA_publications_extraction_20200501_all.xlsx", sheet("Extraction all") firstrow
			drop if PMID == ""
			rename PMID pmid
			replace pmid = substr(pmid,5,8)
			destring pmid, replace
			format pmid %12.0g
			
			save "$d_proj/data/extraction_working_dataset.dta", replace
			
			clear 
			use "$d_proj/data/position_2.dta"
			merge m:m pmid using "$d_proj/data/extraction_working_dataset.dta", force
			sort pmid
			drop if _merge == 2
			drop _merge
		
		/** Remove paper with more than 100 authors
		
		bysort pmid: gen max =_N
		drop if max > 100				// removed because of bad data quality and contribution to the paper might be minor (100 is arbitrary)
			*Carole: we actually don't have any papers with more than 100 authors anymore. My guess is that they have been deleted already in the "**Manually drop of collaboratores, etc."
*/
		drop if pmid == 26079662    	// wrongly on the list of IeDEA papers
		drop if pmid == 33225559 		// wrongly on the list of IeDEA papers 
		
		save "$d_proj/data/position_3.dta", replace
			*Carole: 
				*PMIDs: N = 316 
				*Gender: N = 3,428 (known=2,761, unknown=667)

****** Merge with manual gender

			** Import list of unknown genders
			clear
			import excel "$d_proj/source/temporaire_gender.xlsx", sheet("Sheet1") firstrow			
			rename RowLabels given_name
			rename Gender gender_manual
				
			** Manual cleaning of comments
			
			replace gender_manual = "tobedone" if gender_manual == ""
			replace gender_manual = "mixed" if gender_manual == "mixte"
			drop if given_name == "f.chite"
			drop if given_name == "(blank)"	
			drop if given_name == "Grand Total"	
			drop if given_name == "mme"
			drop if gender_manual == "???"
			drop if gender_manual == "?"
			drop if gender_manual == "tobedone"
			drop if gender_manual == "mixte"
			drop if gender_manual == "mixed"
			replace gender_manual = "male" if gender_manual == "male "
		
			save "$d_proj/data/manual_gender_20200902.dta", replace
		
		** Merge manual genders to dataset
		
		clear
		use "$d_proj/data/position_3.dta"

		merge m:m given_name using "$d_proj/data/manual_gender_20200902.dta"
		drop if _merge == 2
		drop _merge
		
		** Replace inital gender variable where unknown with manual information
		gen gender_check = .
		replace gender_check = 1 if gender != gender_manual 
		replace gender_check = . if gender == "unknown"
		replace gender_check = . if gender_manual == ""		
		replace gender_check = . if gender_manual == "tobedone"		
		replace gender_check = . if gender_manual == "mixte"		
		
		replace gender = gender_manual if gender == "unknown" & gender_manual != "" 

		save "$d_proj/data/position_4.dta", replace
		
		**Import list of manual gender (Kathleen and Carole) and merge
		clear
		import excel "$d_proj/data/missing_gender_20210607_cleaned.xlsx", firstrow
		
		merge m:m pmid surname using "$d_proj/data/position_4.dta"
		drop v1
		drop if _merge == 1
		replace gender = gender_g if gender=="unknown" & gender_g!=""
		replace given_name = given_name_g if given_name=="unknown"
		
		drop _merge
		drop gender_g
		drop given_name_g
		
		save "$d_proj/data/full_gender.dta", replace
		
****** Cleaning of the surnames
		
		** Change capital letters in lower case, remove "accents" and dots and '
		
		gen authorscount = surname + given_name
		
		replace surname = lower(ustrto(ustrnormalize(surname, "nfd"), "ascii", 2))
		replace surname = subinstr(surname, "'", "",.)
		generate dropstr  = strpos(surname,". ")			
		replace surname = substr(surname,4,.) if dropstr != 0
		drop dropstr
		save "$d_proj/data/surname1.dta", replace

		** To remove first last name if there is a "-" in the name (to get rid of composed wedding names)
		
		generate splitsurname  = strpos(surname,"-")
		generate str1 surname_part1 = "" 
		replace surname_part1 = substr(surname,1,splitsurname - 1)
		generate str1 surname_part2 = "" 
		replace surname_part2 = substr(surname,splitsurname + 1,.)
		drop surname_part1
		drop splitsurname
		save "$d_proj/data/surname2.dta", replace

		** Replace surnames with a number to exclude typos
		
		strgroup surname_part2, gen(surname_code) threshold (0.25)
		sort surname_code				
		save "$d_proj/data/surname3.dta", replace	
			
****** Cleaning of the given names

		** Change capital letters in lower case and remove accents
		
		replace given_name = lower(ustrto(ustrnormalize(given_name, "nfd"), "ascii", 2))
		save "$d_proj/data/given_name1.dta", replace	
			
		** To complete the unknown given_names if they are known elsewhere
		
		gen n_given_names = 1
		collapse (count) n_given_names, by (given_name surname_code)
		drop if given_name == "unknown"
		rename given_name given_name_2		
		save "$d_proj/data/given_name_working_dataset.dta", replace
		
		clear 
		use "$d_proj/data/given_name1.dta"	
		merge m:m surname_code using "$d_proj/data/given_name_working_dataset.dta"	
		replace given_name = given_name_2 if given_name == "unknown" & given_name_2 != ""

		drop _merge
		drop given_name_2
		drop n_given_names
					
		save "$d_proj/data/given_name2.dta", replace
			
****** Gender (02.09.2020)
		
		/** To complete the unknown gender if they are known elsewhere
		
		gen n_gender = 1
		collapse (count) n_gender, by (gender given_name)
		drop if gender == "unknown"
		rename gender gender_2
		save "$d_proj/data/gender_working_dataset.dta", replace
		
		clear 
		use "$d_proj/data/given_name2.dta"	
		merge m:m given_name using "$d_proj/data/gender_working_dataset.dta"	
		replace gender = gender_2 if gender == "unknown" & gender_2 != ""

		drop _merge
		drop gender_2
		drop n_gender	*/		
		
		save "$d_proj/data/gender1.dta", replace

		** Coding of gender variables
		
		gen gender_num = 1 if gender == "female"
		replace gender_num = 0 if gender == "male"

		drop gender_manual
		drop gender_check
		save "$d_proj/data/gender2.dta", replace
		
****** Given_names
			
		** To remove middle names and reduce to one name (still to be cleaned if a Quadrat (éèà), ' ,( is in the namepart)

		generate splitname1  = strpos(given_name," ")
		generate str1 name_part1 = "" 
		replace name_part1 = substr(given_name,1,splitname1 - 1)
		generate str1 name_part2 = "" 
		replace name_part2 = substr(given_name,splitname1 + 1,.)
		
		gen ndot = length(name_part1) - length(subinstr(name_part1, ".", "", .))
		replace name_part1 = name_part2 if ndot ==1 | ndot == 2

		gen ndigit = strlen(name_part1) 
		replace name_part1 = name_part2 if ndigit ==1
		
		replace name_part1 = name_part2 if name_part1 == ""
		
		drop name_part2 ndigit ndot splitname1
		save "$d_proj/data/given_name3.dta", replace

		strgroup name_part1, gen(given_name_code) threshold (0.25)
		sort given_name_code		
	
					
		** Combining surnames and first names (surname_code and name_short) to generate one code per author
		
		tostring surname_code, replace
		tostring given_name_code, replace
		gen author_code = given_name_code + surname_code
		*to see missing author's first names
		tab author_code if strpos(author_code, "unknown")
		
		save "$d_proj/data/given_name4.dta", replace	
					
****** Affiliations (07.09.2020)
		
		** Identify the missings and screen for bias		
		*save "$d_proj/data/excel1.dta", replace
							gen n_affil = 1
					collapse (count) n_affil, by (affil_country author_code)
					drop if affil_country == "unknown"
					rename affil_country affil_country_2
					save "$d_proj/data/affil_country_working_dataset.dta", replace
					
					clear 
					use "$d_proj/data/given_name4.dta"	
					merge m:m author_code using "$d_proj/data/affil_country_working_dataset.dta"	
					replace affil_country = affil_country_2 if affil_country == "unknown" & affil_country_2 != ""
					
					drop _merge			
					drop n_affil
					drop affil_country_2
					
					save "$d_proj/data/affil1.dta", replace

					** Take most frequent affiliation per year and person
			
					/*gen n_affil = 1
					collapse (count) n_affil, by (affil_country author_code publicationyear)
					drop if affil_country == "unknown"
					egen affil_country_year_max = max(n_affil), by(surname_code given_name_code publicationyear)
					keep if affil_country_year_max == n_affil
					gen affil_author_year = affil_country
					save "$d_proj/data/affil_country_working_dataset.dta", replace
					
					clear 
					use "$d_proj/data/excel1.dta"	
					merge m:m surname_code given_name_code publicationyear using "$d_proj/data/affil_country_working_dataset.dta"	
					*replace affil_country = affil_country_2 if affil_country == "unknown" & affil_country_2 != ""
					
					drop _merge			
					drop n_affil
					drop affil_country_year_max
					
					save "$d_proj/data/affil1.dta", replace
					
					
				** Take most frequent affiliation per person for the whole data period
			
					gen n_affil_2 = 1
					collapse (count) n_affil_2, by (affil_country surname_code given_name_code)
					drop if affil_country == "unknown"
					egen affil_country_max = max(n_affil_2), by(surname_code given_name_code)
					keep if affil_country_max == n_affil_2
					gen affil_author = affil_country
					save "$d_proj/data/affil_country_working_dataset_2.dta", replace
					
					clear 
					use "$d_proj/data/affil1.dta"	
					merge m:m surname_code given_name_code using "$d_proj/data/affil_country_working_dataset_2.dta"	
					*replace affil_country_max_year = affil_country_max_year_2 if affil_country_max_year == ""
					
					drop _merge			
					drop n_affil_2
					drop affil_country_max
					
					save "$d_proj/data/affil2.dta", replace
							
					*/
** Merge with low-, middle- and high income
								
					** Generate categories by the data from the worldbank
					clear
					import excel "$d_proj/source/CLASS.xls", sheet("List of economies") firstrow
					keep Economy Incomegroup
					rename Economy affil_country
					replace affil_country = "Cote d'Ivoire" if affil_country == "Côte d'Ivoire"
					replace affil_country = "Viet Nam" if affil_country == "Vietnam"
					replace affil_country = "Hong Kong" if affil_country == "Hong Kong SAR, China"
					replace affil_country = "Taiwan" if affil_country == "Taiwan, China"
					replace affil_country = "South Korea" if affil_country == "Korea, Rep."
					replace affil_country = "Democratic Republic Congo" if affil_country == "Congo, Dem. Rep."
					replace affil_country = "Iran" if affil_country == "Iran, Islamic Rep."
					save "$d_proj/data/class_working_dataset.dta", replace
					
					** Merge worldbank with our data
					clear 
					use "$d_proj/data/affil1.dta"
					replace affil_country = "Democratic Republic Congo" if affil_country == "Congo"
					*replace affil_author_year = "Democratic Republic Congo" if affil_country == "Congo"
					*replace affil_author = "Democratic Republic Congo" if affil_country == "Congo"
					replace affil_country = "Eswatini" if affil_country == "Swaziland"
					*replace affil_author_year = "Eswatini" if affil_country == "Swaziland"
					*replace affil_author = "Eswatini" if affil_country == "Swaziland"
					merge m:m affil_country using "$d_proj/data/class_working_dataset.dta"
					sort pmid
					drop if pmid ==.

					gen class_num = 1 if Incomegroup == "Low income"
					replace class_num = 2 if Incomegroup == "Lower middle income"
					replace class_num = 3 if Incomegroup == "Upper middle income"
					replace class_num = 4 if Incomegroup == "High income"
					replace class_num = . if Incomegroup == "Unknown"
					drop _merge

					label define Class 1 "Low income" 2 "Lower middle" 3 "Upper middle" 4 "High income"
					label values class_num Class
					label define Gender 0 "Male" 1 "Female" 99 "Unknown"
					label values gender_num Gender	
		
					save "$d_proj/data/dataset_for_analyses.dta", replace					
