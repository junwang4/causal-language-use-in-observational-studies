#------------------------------------
#
load_packages = function() {
	for (pkg in c("argparse", "data.table", "lme4", "tictoc", "dplyr", "tidyr", "stringr", "ggplot2", "ggrepel", "ggbreak")) {
		suppressMessages(require(pkg, character.only = TRUE))
	}
}

#------------------------------------
#
create_filename_for_output = function(namebase, ext='pdf', folder='working/report/pdf') {
	ifelse(!dir.exists(folder), dir.create(folder, recursive=TRUE), FALSE)
	filename = paste(c(namebase, '.', ext), collapse = '')
	return (paste(c(folder, filename), collapse = '/') )
}


process_gender <- function(vec, male_min_confidence, female_min_confidence) {
  fcase(
    is.na(vec),           NA_character_,
    vec < 0,              "I",  # initial only
    vec > male_min_confidence,           "M",
    vec < 1 - female_min_confidence,    "F",
    default = "L"  # Low-confidence 
  )
}

preprocess_gender_first_and_last <- function(df, genderTH='M82_F78') {
	male_min_confidence <- as.numeric(str_extract(genderTH, "(?<=M)\\d+")) / 100
	female_min_confidence <- as.numeric(str_extract(genderTH, "(?<=F)\\d+")) / 100
	cat(sprintf('\n- male_min_confidence = %s\n', male_min_confidence))
	cat(sprintf('- female_min_confidence = %s\n', female_min_confidence))

	df[, `:=`(
  		gender_first = process_gender(gender_first, male_min_confidence, female_min_confidence),
  		gender_last  = process_gender(gender_last, male_min_confidence, female_min_confidence)
	)]
	invisible(df)
}