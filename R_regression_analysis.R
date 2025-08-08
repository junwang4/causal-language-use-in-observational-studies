#test_year_as_control_instead_of_random_effect = TRUE  # in case we want to test year as a fixed effect instead of a random effect
test_year_as_control_instead_of_random_effect = FALSE

source("./R_helpers.R")

#------------------------------------
#
load_and_preprocess_data = function(fpath_data, fpath_mesh_term, add_mesh_terms=T, meshtermTH=150, genderTH='M80_F80') {
	cat(sprintf('- read main data from: %s\n', fpath_data))
	cat(sprintf('- read mesh term from: %s\n', fpath_mesh_term))

	#male_min_confidence <- as.numeric(str_extract(genderTH, "(?<=M)\\d+")) / 100
	#female_min_confidence <- as.numeric(str_extract(genderTH, "(?<=F)\\d+")) / 100
	#cat(sprintf('- male_min_confidence = %s\n', male_min_confidence))
	#cat(sprintf('- female_min_confidence = %s\n', female_min_confidence))

	df = fread(fpath_data)
	cat(paste0('- number of papers: ', nrow(df), '\n'))

	df_term = fread(fpath_mesh_term)

	# create a list of PMID–term pairs for analysis, based on a frequency threshold.
	df_term = df_term[cnt >= meshtermTH]
	df_term <- df_term[, .(pmid = unlist(strsplit(pmid_list, "\t", fixed = TRUE))), by = term]
	df_term <- df_term[, .(pmid = as.numeric(pmid), term)]

	cat(paste0('- number of distinct mesh terms: ', uniqueN(df_term$term), '\n'))
	if (!add_mesh_terms) {
		selected_terms <- c("Cross-Sectional Studies", "Case-Control Studies", 
                    "Cohort Studies", "Longitudinal Studies", 
                    "Prospective Studies", "Retrospective Studies", 
                    "Follow-Up Studies")
		df_term = df_term[term %in% selected_terms]
	}

    preprocess_gender_first_and_last(df, genderTH)

	# merge gender (I: initial only, L: low-confidence) to be one group: U
    df[, gender_first:= ifelse(gender_first %in% c('M', 'F'), gender_first, 'U')]
    df[, gender_last:= ifelse(gender_last %in% c('M', 'F'), gender_last, 'U')]

  	missing_count <- sum(is.na(df$sjr))
	cat(sprintf('\n- number of papers that do not have SciMago Journal Rank (sjr): %d\n', missing_count))

	# impute missing sjr with median (in terms of journals instead of all articles)
	dt = df[, .(sjr = median(sjr, na.rm = TRUE)), by = journal]
	sjr_median_by_journal = median(dt$sjr, na.rm = TRUE)
	sjr_median = median(df$sjr, na.rm = TRUE)
	cat(sprintf('\n- sjr_median: %.3f\n', sjr_median))
	cat(sprintf('- sjr_median_by_journal: %.3f\n', sjr_median_by_journal))

	# calculate the median of the bottom quarter of the sjr by journal
	dt = df[, .(sjr = median(sjr, na.rm=TRUE)), by = journal]
	sjr_bottom_quarter_by_journal = quantile(dt$sjr, 0.25, na.rm = TRUE)
	cat(sprintf('- sjr_bottom_quarter_by_journal: %.3f\n', sjr_bottom_quarter_by_journal))
	cat(sprintf('- unique journals: %d\n', uniqueN(dt$journal)))

	# papers without sjr suggest their journals' sjr rank is relatively low,
	# so we fill their sjr with the bottom quarter of the sjr by journal
	df[is.na(sjr), sjr := sjr_bottom_quarter_by_journal]
	cat(sprintf('  we use sjr_bottom_quarter_by_journal to fill missing sjr\n'))

	# define the outcome variable "has_causal"
	df[, has_causal:= ifelse(claim == 'causal', 1, 0)]
	cat('\n- claim distribution:\n')
	print(table(df$claim))

  	cat('\n- year distribution:\n')
	print(table(df$year))
	if (test_year_as_control_instead_of_random_effect) {
        df[year <= 2013, year := 2013]
        df[year >= 2025, year := 2025]
  	}
	print(table(df$year))

	df = log_transform_for_columns_with_skewed_distribution(df)
	df = convert_year_and_sentence_cnt_to_factor(df)
	df = generate_column_identifiers_for_easy_postprocessing(df, 'country')

	# populate the main data with mesh terms (a paper usually has many mesh terms)
	df_term = generate_column_identifiers_for_easy_postprocessing(df_term, 'term')

  	# for each paper, create a column for each mesh term
	df_term = df_term %>% mutate(value = 1)  %>% spread(term, value,  fill = 0)
	setDT(df_term)  # Convert to data.table if it's a data.frame

	cat('\n- before merge with df_term:', nrow(df), '' )
	df = merge(df, df_term, all.x=T, by='pmid') # in case some papers dont have mesh terms
	df[is.na(df)] <- 0
	cat('\n-  after merge with df_term:', nrow(df), '   // make sure they are the same\n' )

    df = generate_column_identifiers_for_easy_postprocessing(df, 'journal')

	display_statistics(df)

	meshterm_list_as_formula = paste(names(df_term)[2:length(df_term)], collapse = " + ")  # the first is pmid, so skip it
	return(list(df, meshterm_list_as_formula))
}


#------------------------------------
#
log_transform_for_columns_with_skewed_distribution = function(df) {
    cat('\n- run log-transform for columns with skewed distribution:\n\n  sjr, first_author_paper_cnt, last_author_paper_cnt, num_author, journal_paper_cnt\n')

	df[, sjr_log:= log1p(sjr)] 

	df[, first_author_paper_cnt:=.N, by = list(author_id_first)]
	df[, last_author_paper_cnt:=.N, by = list(author_id_last)]

	df[, first_author_paper_cnt_log:= log1p(first_author_paper_cnt)]
	df[, last_author_paper_cnt_log:= log1p(last_author_paper_cnt)]

	df[, num_author_log:= log1p(num_author)]

	df[, journal_paper_cnt:=.N, by = list(journal)]
	df[, journal_paper_cnt_log:= log1p(journal_paper_cnt)]

	return(df)
}

#-----------------
#
generate_column_identifiers_for_easy_postprocessing = function(df, col) {
	if (col=='country') {
		df$country = paste0('c_', gsub('\\W+', '', df$country))
	} else if (col=='term') {
		df$term = paste0('t_', gsub('\\W+', '', df$term))
	} else if (col=='journal') {
		df$journal = paste0('j_', gsub('\\W+', '', df$journal))
	}
	return(df)
}

#-----------------
#
display_statistics = function(df) {
	cat('\n- country distribution (c_: all others as reference):\n')
	print(data.table(table(df$country))[order(N)])

	cat("\n- number of unique journals:", uniqueN(df$journal))
	cat('\n- journal distribution:\n')
	print(data.table(table(df$journal))[order(-N)][1:10])
	#print(data.table(table(df$journal))[order(N)][1:5])

	cat('\n- first-author gender (U: low-confidence or initial only):')
	print(table(df$gender_first))

	cat('\n- last-author gender (U: low-confidence or initial only):')
	print(table(df$gender_last))

	cat('\n- num of sentences in the abstract conclusion: ')
	print(table(df$sentence_cnt))

	cat('\n- num of records:', nrow(df), '\n' )
}

#-----------------
#
convert_year_and_sentence_cnt_to_factor = function(df) {
    cat('\n- convert numeric type to factor: year, sentence_cnt\n')
	cols = c('year', 'sentence_cnt')
	for (j in cols) {
		set(df, j = j, value = factor(df[[j]]))
	}
	return(df)
}


#------------------------------------
#
display_summary_of_model = function(model) {
	co = summary(model)$coefficients
	names = row.names(co)
  
	# remove the mesh_term parts since there are over hundreds of them
	tmp = startsWith(names, 't_')
	co = co[!(row.names(co) %in% names[tmp]), ]
	cat('\n- model summary (meshterm effects are not shown here; check the output file for details)\n\n')

	colnames(co) = c('Est', 'StdErr', 'z', 'p_val')
	co = data.frame(co)
	co$sig_level = ifelse(co$p<0.001, '***', ifelse(co$p<0.01, '**', ifelse(co$p<0.05, '*', ifelse(co$p<0.1, '.', ''))))
  
	co[, 1:4] <- round(co[, 1:4], digits = 5)

	print.data.frame(co)
	cat('\n')
}

#------------------------------------------------
#
append_factor = function(fixed_parts, factor) {
	if (str_length(fixed_parts) == 0)
		return(paste0(" ", factor, " "))
	else
		return(paste0(fixed_parts, " + ", factor, " "))
}

#------------------------------------
#
run_regression_model = function(data,
                                meshterm_list_as_formula,
								add_year=T,
								add_journal=T,
								add_mesh_terms=T,
								add_author_paper_cnt=T,
								add_journal_paper_cnt=T,
								add_sjr=T,
								add_num_of_authors=T,
								add_gender=T,
								add_country=T) {
  
	fixed_parts = ''

    if (add_mesh_terms) {
		fixed_parts = append_factor(fixed_parts, meshterm_list_as_formula)
	} else {
		fixed_parts = append_factor(fixed_parts, "t_CrossSectionalStudies + t_CaseControlStudies + t_CohortStudies + t_LongitudinalStudies + t_RetrospectiveStudies+ t_ProspectiveStudies + t_FollowUpStudies ")
	}

	if (add_country) {
		fixed_parts = append_factor(fixed_parts, 'country ')
	}

    if (add_sjr) {
		fixed_parts = append_factor(fixed_parts, "sjr_log")
	}
    
	if (add_author_paper_cnt) {
		fixed_parts = append_factor(fixed_parts, "first_author_paper_cnt_log + last_author_paper_cnt_log")
	}

	if (add_num_of_authors) {
		fixed_parts = append_factor(fixed_parts, " num_author_log ")
	}

	if (add_journal_paper_cnt) {
		fixed_parts = append_factor(fixed_parts, " journal_paper_cnt_log ")
	}

	if (add_gender) {
		fixed_parts = append_factor(fixed_parts, " gender_first + gender_last ")
	}

  	random_parts = "(1 | year) + (1 | sentence_cnt)"
  	if (test_year_as_control_instead_of_random_effect) {
  	    random_parts = "(1 | sentence_cnt)"
		fixed_parts = append_factor(fixed_parts, " year ")
  	}

    if (add_journal) {
	    random_parts = paste0(random_parts, " + (1 | journal)")
	}

	# run the regression model
	tic()
	y = 'has_causal'
	formula = as.formula(paste(c(y, " ~ ", paste(c(fixed_parts, random_parts), collapse="+")), collapse="") )
	display_formula(formula)

	m = glmer(formula, data=data, family = binomial, control = glmerControl(optimizer = "bobyqa"), nAGQ=0)

	cat('\n- done with glmer.\n')
	toc()

	display_summary_of_model(m)
	return(m)
}

#---------------
#
display_formula = function(formula) {
    #cat(sprintf("\n- formula: %s\n\n", toString(formula)))
	long_string = toString(formula)
	first_100 <- substr(long_string, 1, 50)
    last_300 <- substr(long_string, nchar(long_string) - 210, nchar(long_string))
	cat(sprintf("\n- formula: %s ...\n  ... %s\n\n", first_100, last_300))
}

#------------------------------------
#
#save_regression_model = function(model_overall, add_journal, add_mesh_terms, meshtermTH, genderTH, model_output_dir) {
save_regression_model = function(model_overall, fpath_model_coeff, fpath_model_param) {
	summ = summary(model_overall)

    # In R, global variables are accessible from functions defined in other scripts as long as they are defined in the global environment.
	num_obs = nrow(df)

	write.table(summ$coefficients, file=fpath_model_coeff, sep=',')
	#cat(sprintf('\n- model coefficients output to: %s\n', fpath_model_coeff))
	cat('\n- model coefficients are saved to:\n')
	cat(fpath_model_coeff)
	cat('\n\n')

	df_param = data.frame(t(summ$AICtab))
	df_param$num_obs = num_obs

	write.table(df_param, file=fpath_model_param, sep=',', row.names=F)
	cat(sprintf('- model AIC and num.obs output to: %s\n', fpath_model_param))
}
