#------------------------------------
#
load_packages = function() {
	for (pkg in c("data.table", "lme4", "tictoc", "dplyr", "tidyr", "stringr", "ggplot2")) {
		suppressMessages(require(pkg, character.only = TRUE))
	}
}

#test_year_as_control_instead_of_random_effect = TRUE
test_year_as_control_instead_of_random_effect = FALSE

#------------------------------------
#
create_filename_for_output = function(namebase, ext='csv', folder_output='working') {
	ifelse(!dir.exists(folder_output), dir.create(folder_output, recursive=TRUE), FALSE)
	filename = paste(c(namebase, '.', ext), collapse = '')
	return (paste(c(folder_output, filename), collapse = '/') )
}

#------------------------------------
#
load_and_preprocess_data = function(fpath_data, fpath_mesh_term) {
	cat(sprintf('- read main data from: %s\n', fpath_data))
	cat(sprintf('- read mesh term from: %s\n', fpath_mesh_term))

	df = fread(fpath_data)
	cat(paste0('- number of papers: ', nrow(df), '\n'))

	df_term = fread(fpath_mesh_term)
	cat(paste0('- number of distinct mesh terms: ', uniqueN(df_term$term), '\n'))

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
    }
	print(table(df$year))

	df = log_transform_for_columns_with_skewed_distribution(df)
	df = convert_year_and_sentence_cnt_to_factor(df)
	df = generate_column_identifiers_for_easy_postprocessing(df, 'country')

	# populate the main data with mesh terms (a paper usually has many mesh terms)
	df_term = generate_column_identifiers_for_easy_postprocessing(df_term, 'term')

    # for each paper, create a column for each mesh term
	df_term = df_term %>% mutate(value = 1)  %>% spread(term, value,  fill = 0)

	cat('\n- before merge with df_term:', nrow(df), '' )
	df = merge(df, df_term, all.x=T, by='pmid') # in case there are papers without mesh terms
	cat('\n-  after merge with df_term:', nrow(df), '   // make sure they are the same\n' )

    df = generate_column_identifiers_for_easy_postprocessing(df, 'journal')

	display_statistics(df)

	factor_of_mesh_terms = paste(names(df_term)[2:length(df_term)], collapse = " + ") # the first is pmid, so skip it
	return(list(df, factor_of_mesh_terms))
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
	} else if (col=='jouranl') {
		df$journal = paste0('j_', gsub('\\W+', '', df$journal))
	}
	return(df)
}

#-----------------
#
display_statistics = function(df) {
	cat('\n- country distribution (c_: all others):\n')
	print(data.table(table(df$country))[order(N)])

	cat('\n- journal distribution (top 5 and bottom 5):\n')
	print(data.table(table(df$journal))[order(-N)][1:5])
	print(data.table(table(df$journal))[order(N)][1:5])

	cat('\n- first author gender (L: low-confidence and initial only):')
	print(table(df$gender_first))

	cat('\n- last author gender (L: low-confidence; I: initial only):')
	print(table(df$gender_last))

	cat('\n- num of sentences in the abstract conclusion: ')
	print(table(df$sentence_cnt))

	cat('\n- num of records:', nrow(df), '\n' )
}

#-----------------
#
convert_year_and_sentence_cnt_to_factor = function(df) {
    cat('\n- convert number to factor: year, sentence_cnt\n')
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
	cat('\n- mesh_term effects are not shown here (see the output file instead)\n\n')

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
                                mesh_terms,
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
		fixed_parts = append_factor(fixed_parts, mesh_terms)
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
save_regression_model = function(model_overall, add_journal, add_mesh_terms, folder_model_output) {
	summ = summary(model_overall)

    # In R, global variables are accessible from functions defined in other scripts as long as they are defined in the global environment.
	num_obs = nrow(df)

	model_no = sprintf('size%s_journal%s_meshterm%s', num_obs, add_journal, add_mesh_terms)

	model_no = gsub("TRUE", "T", gsub("FALSE", "F", model_no))

	fpath_model_coeff = create_filename_for_output(sprintf('coef_%s', model_no), "csv", folder=folder_model_output)
	cat('\n\n- for easy copy-paste, the model coefficients are saved to:\n')
	cat(fpath_model_coeff)
	cat('\n')

	write.table(summ$coefficients, file=fpath_model_coeff, sep=',')
	cat(sprintf('\n- model coefficients output to: %s\n', fpath_model_coeff))

	df_param = data.frame(t(summ$AICtab))
	df_param$num_obs = num_obs

	fpath_model_param = create_filename_for_output(sprintf('param_%s', model_no), "csv", folder=folder_model_output)

	write.table(df_param, file=fpath_model_param, sep=',', row.names=F)
	cat(sprintf('- model AIC and num.obs output to: %s\n', fpath_model_param))
}