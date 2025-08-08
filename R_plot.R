source("./R_helpers.R")

#------------------------------------
#
my_corr_test = function(dt, msg) {
    cor_test <- cor.test(dt$est, dt$UAI, method = "pearson")
    pearson_corr <- cor_test$estimate
    p_value <- cor_test$p.value
    cat(sprintf("\n%s    Correlation: %.2f    p-value: %.5f", msg, pearson_corr, p_value))
}

save_fig = function(figname, w = 5, h = 3) {
	ggsave(figname, plot=last_plot(), width=w, height=h, dpi=300)
	cat(sprintf('\n- output saved to file: \n  %s\n', figname))
}

display_misc = function() {
    display_gender_distri_by_study_design(df_main)
}

display_gender_distri_by_study_design = function(df) {
    #attr = "t_ProspectiveStudies"
    #attr = "t_FollowUpStudies"
    #attr = "t_CaseControlStudies"
    attr = "t_CrossSectionalStudies"
    table1 <- table(df$gender_first, df[[attr]])
    #df <- df[df$gender_last != "U", ]
    table2 <- table(df$gender_last, df[[attr]])
    print(table1)
    print(table2)

    print(chisq.test(table1))  # Test for first author gender vs. study design
    print(chisq.test(table2))  # Test for last author gender vs. study design
}


#------------------------------------
#
plot_country_uncertainty_avoidance_index_vs_causal_language_use = function(fpath_UAI, fpath_data, fpath_model_coeff) {
    dt_uai = fread(fpath_UAI)
    df_main = fread(fpath_data)
	dt_model_coef = fread(fpath_model_coeff, header = FALSE, col.names = c('var', 'est', 'stderr', 'z', 'pval'))

    dt_coef <- dt_model_coef[grepl("^countryc_", var), .(est, country_code = substr(var, nchar(var)-1, nchar(var)))]
    dt = merge(dt_coef, dt_uai, by = "country_code", all.x =  TRUE)
	dt <- na.omit(dt)

	dt_country_cnt <- df_main[, .N, by = country]
	setnames(dt_country_cnt, c("country_code", "sample_size"))
	dt_country_cnt[, sample_size := log(sample_size)]
	dt = merge(dt, dt_country_cnt, by = 'country_code')

    non_western_country_list <- strsplit('CN JP KR TW EG TR IL IN BR MX IR PK SA SG TH CO ZA AR', " ")[[1]]
    dt[, is_western := ifelse(country_code %in% non_western_country_list, F, T)]
    dt_western = dt[!country_code %in% non_western_country_list]
    dt_nonwestern = dt[country_code %in% non_western_country_list]

    print(dt[order(-est), .(country, est, UAI, is_western)])

    my_corr_test(dt, 'all    ')
    my_corr_test(dt_western, 'western')
    my_corr_test(dt_nonwestern, 'non-western')
    cat('\n\n')

    #dt = dt_western 
    #dt = dt_nonwestern

    # https://htmlcolorcodes.com/color-names/
    p <- ggplot(dt, aes(x = UAI, y = est, size=sample_size)) +
        geom_point(aes(shape = is_western, color=is_western, fill=is_western), alpha = 0.7, stroke=1.2) +
		scale_size_continuous(range = c(2, 12)) +
        scale_color_manual(values = c("Salmon", "CornflowerBlue")) +
        scale_shape_manual(values = c(1, 19)) +
        geom_text_repel(aes(label = country), color='#000000', size = 4.0, nudge_x = 0, nudge_y = 0.008) +
				theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"))

    p <- p + labs( x = "Uncertainty Avoidance Index (UAI)", y = "Use of Causal Language") + theme(
        legend.position = "none",
        axis.text = element_text(size = 10),  # Adjust the size of tick labels
        axis.title = element_text(size = 12),  # Adjust the size of axis labels
        plot.title = element_text(size = 14)   # Adjust the size of the plot title
    )

    p <- p + theme( panel.background = element_blank())
	#fpath_output = create_filename_for_output(sprintf('uncertainty_vs_causal'), 'eps')  # (alpha/semi-transparency not supported) use eps because: arxiv submission failed: missing bounding box for pdf
	#save_fig(fpath_output, w=6, h=4.5)
	fpath_output = create_filename_for_output(sprintf('uncertainty_vs_causal'), 'pdf')
	save_fig(fpath_output, w=6, h=4.5)
    cat('\n\n')
}


plot_gender_distribution_by_country = function() {
	df = df_main[country!='::']  # remove the row with country='::' ('others' category)

    melted_df <- melt(df, id.vars = "country", measure.vars = patterns("^gender_"), value.name = "gender")
    print(melted_df)
    df = melted_df
	gender_var_name <- "gender"

	data <- df %>% group_by(country, !!sym(gender_var_name)) %>% summarise(count = n())
	total_counts <- data %>% group_by(country) %>% summarise(total_count = sum(count))

    data <- data %>% left_join(total_counts, by = "country") %>% arrange(total_count)

	ggplot(data, aes(x = factor(country, levels = rev(unique(data$country))), y = count, fill = !!sym(gender_var_name))) +
        geom_bar(stat = "identity", color = "white", linewidth = 0.2) +
		scale_fill_manual(values=c("#F8766D", "#b1cbbb", "#f1c232", "#00B6EB")) +
        labs(title = "", x = "", y = "Count (First and Last Author Genders Combined)") +
        theme(legend.position.inside = c(1, 0.5), legend.justification = c(1, 0.5), legend.box.just = "right",  panel.background = element_blank())

	fpath_output = create_filename_for_output('country_gender', 'pdf')
	save_fig(fpath_output, w=8, h=5)
}

plot_gender_distribution_by_author_position = function(fpath_data) {
    df = fread(fpath_data)
    preprocess_gender_first_and_last(df)

    print(table(df$gender_first))
    print(prop.table(table(df$gender_first)))
    cat('\n')

    cat(sprintf("Proportion of low-confidence (L) authors among first authors (L, F, M): %.3f\n",
                sum(df$gender_first %in% c("L")) / sum(df$gender_first %in% c("L", "F", "M"))))

    cat(sprintf("Proportion of initial only (I) authors among first authors (L, I): %.3f\n",
                sum(df$gender_first %in% c("I")) / sum(df$gender_first %in% c("L", "I"))))

    cat(sprintf("Proportion of initial only (I) or low-confidence (L) authors among all first authors (F, M, L, I): %.3f\n",
                sum(df$gender_first %in% c("I", "L")) / sum(df$gender_first %in% c("F", "M", "L", "I"))))

    print(table(df$gender_last))
    print(prop.table(table(df$gender_last)))
    cat('\n')

    cat(sprintf("Proportion of low-confidence (L) authors among last authors (L, F, M): %.3f\n",
                sum(df$gender_last %in% c("L")) / sum(df$gender_last %in% c("L", "F", "M"))))

    cat(sprintf("Proportion of initial only (I) authors among last authors (L, I): %.3f\n",
                sum(df$gender_last %in% c("I")) / sum(df$gender_last %in% c("L", "I"))))

    cat(sprintf("Proportion of initial only (I) or low-confidence (L) authors among all last authors (F, M, L, I): %.3f\n",
                sum(df$gender_last %in% c("I", "L")) / sum(df$gender_last %in% c("F", "M", "L", "I"))))

    df[, gender_first:= ifelse(gender_first %in% c('M', 'F'), gender_first, 'U')]
    df[, gender_last:= ifelse(gender_last %in% c('M', 'F'), gender_last, 'U')]

	dt = melt(df[, .(pmid, gender_first, gender_last)], id='pmid')
    dt$value <- factor(dt$value, levels = c("F", "U", "M"))
	colnames(dt) = c('pmid', 'first_or_last', 'gender')
	dt[, first_or_last:=ifelse(first_or_last=='gender_first', 'First author', 'Last author')]

    dt$gender <- recode(dt$gender, "F" = "Female", "M" = "Male", "U" = "Unknown")

	ggplot(data=dt, aes(x=first_or_last, fill=gender)) + 
        #geom_bar(stat="count", position="dodge") +
        geom_bar(stat = "count", position = position_dodge2(preserve = "single"), width=0.9) + 
        scale_x_discrete(expand = c(0, 0.45)) + 

        geom_text(aes(label = gender, y = after_stat(count)),
                    stat = "count", position = position_dodge(width = 0.9), vjust = -0.5, size = 2.8) +  # Add labels on top
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = seq(10000, 60000, by = 10000),) +  # Add extra space above the bars

		scale_fill_manual(values=c("#F8766D", "#f1c232", "#00B6EB"),
                          labels = c("F"="Female", "U"="Unknown", "M"="Male"),
                          name = NULL) +
		xlab('') + ylab('') +
        #scale_y_continuous(expand = c(0, 0)) +
		#theme(panel.grid = element_blank(),  panel.background = element_blank(), plot.margin = unit(c(0.2, 0.2, -0.7, 0.2), "lines"))
		theme(panel.grid = element_blank(),  panel.background = element_blank(), plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")) +
          theme(legend.position = "none") + 
					    theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"))


	fpath_output = create_filename_for_output(sprintf('gender_distribution'), 'pdf')
	save_fig(fpath_output, w=3.6, h=2.5)
    cat('\n\n')
}

#------------------------------------
#
plot_y_distribution = function(df) {
	df_counts <- df[, .N, by = 'claim']
	df_counts[, claim := sub("associational", "correlational", claim)]  
	print(df_counts)

	ggplot(df_counts, aes(x = reorder(claim, -N), y = N, fill = claim)) +
  		geom_bar(stat = "identity", show.legend = FALSE, fill = "#56B4E9", width=0.6) +  # Remove legend
		scale_y_continuous(breaks = seq(10000, 60000, by = 10000),) +
  		#geom_text(aes(label = N), vjust = -0.5, size = 5) +  # Add count labels on top
  		labs(x = "", y = "") +
  		theme_minimal() +
		theme(panel.grid = element_blank(),  panel.background = element_blank()) +
		theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"))
	fpath_output = create_filename_for_output(sprintf('y_distribution'), 'pdf')
	save_fig(fpath_output, w=3.5, h=2.2)
}

#------------------------------------
#
plot_country_distribution = function(df) {
	#others_cnt = nrow(df[country=='::'])
	#df = df[country!='::']
    #annotation = sprintf('articles by authors from other countries, multiple countries, or unknown countries: %s', format(others_cnt, big.mark = ","))

	df[ , country := gsub("::", "...", country)]
	df <- df[country != "..."]

	df = df[, .(count=.N), by = list(country)][order(-count)]
	df$country <- reorder(df$country, -df$count)
	print(df)

    df_country_code = fread('csv/UAI.csv') # country, country_code
    setnames(df_country_code, old = c("country", "country_code"), new = c("name", "code"))

    dt <- merge(df[, .(country)], df_country_code[, .(name, code)], by.x = "country", by.y = "code")
    print(dt)

    midpoint_1 <- round(1/4 * nrow(dt))
    midpoint_2 <- round(2/4 * nrow(dt))
    midpoint_3 <- round(3/4 * nrow(dt))
	print(midpoint_1)
	print(midpoint_2)

    # Split the data table into two halves
    #dt1 <- dt[1:midpoint, ]
    #dt2 <- dt[(midpoint + 1):nrow(dt), ]
    dt1 <- dt[1:midpoint_1, ]
    dt2 <- dt[(midpoint_1 + 1):midpoint_2, ]
    dt3 <- dt[(midpoint_2 + 1):midpoint_3, ]
    dt4 <- dt[(midpoint_3 + 1):nrow(dt), ]

    country_list_text1 <- paste0(dt1$country, collapse = "\n")
    country_list_text1_2 <- paste0(' ', dt1$name, collapse = "\n")
    country_list_text2 <- paste0(dt2$country, collapse = "\n")
    country_list_text2_2 <- paste0(' ', dt2$name, collapse = "\n")
    country_list_text3 <- paste0(dt3$country, collapse = "\n")
    country_list_text3_2 <- paste0(' ', dt3$name, collapse = "\n")
    country_list_text4 <- paste0(dt4$country, collapse = "\n")
    country_list_text4_2 <- paste0(' ', dt4$name, collapse = "\n")
    cat('\n\n')
    cat(country_list_text1)
    cat('\n\n')
    cat(country_list_text2)
    cat('\n\n')
    cat(country_list_text3)
    cat('\n\n')
    cat(country_list_text4)

    annotation = sprintf('articles by authors from other countries, multiple countries, or unknown countries')
    #country_list_text = 'US: United States\nJP: Japan\nCN: China\nTW: Taiwan'

	font1 = "Courier"
	font2 = "Times"
	font1 = "sans"
	font2 = "sans"
  fs1 = 3.6	
  fs2 = 3.6	
	y_base = 2000
	ggplot(data=df, aes(x=country, y=count)) +
		geom_bar(stat="identity", fill='#56B4E9') +
		scale_y_continuous(breaks = seq(0, 14000, by = 500)) +
		scale_y_break(c(5000, 13000)) +

		#geom_text(aes(label = count), vjust = -0.3, size=3) +
		#xlab('Country code') + ylab('Number of Observational Studies') +
		xlab('') + ylab('Number of Observational Studies') +
		geom_segment(x = as.factor('CN'), y = 20500, xend = as.factor('US'), yend = 20500, arrow = arrow(length = unit(0.2, "cm"))) +

		geom_text(family = font1, label = country_list_text1, check_overlap = TRUE, x = as.factor('BR'), size = fs1, y = y_base, vjust = 0, hjust = 0) +
		geom_text(family = font2, label = country_list_text1_2, check_overlap = TRUE, x = as.factor('CA'), size = fs2, y = y_base, vjust = 0, hjust = 0) +
		geom_text(family = font1, label = country_list_text2, check_overlap = TRUE, x = as.factor('IL'), size = fs1, y = y_base, vjust = 0, hjust = 0) +
		geom_text(family = font2, label = country_list_text2_2, check_overlap = TRUE, x = as.factor('PL'), size = fs2, y = y_base, vjust = 0, hjust = 0) +
		geom_text(family = font1, label = country_list_text3, check_overlap = TRUE, x = as.factor('MX'), size = fs1, y = y_base, vjust = 0, hjust = 0) +
		geom_text(family = font2, label = country_list_text3_2, check_overlap = TRUE, x = as.factor('FI'), size = fs2, y = y_base, vjust = 0, hjust = 0) +
		geom_text(family = font1, label = country_list_text4, check_overlap = TRUE, x = as.factor('SA'), size = fs1, y = y_base, vjust = 0, hjust = 0) +
		geom_text(family = font2, label = country_list_text4_2, check_overlap = TRUE, x = as.factor('SG'), size = fs2, y = y_base, vjust = 0, hjust = 0) +

		theme(panel.grid = element_blank(),  panel.background = element_blank()) +
		theme(axis.ticks.y.right = element_blank(), axis.text.y.right = element_blank()) +
		theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"))


	figname = create_filename_for_output(sprintf('country_distribution'), 'pdf')
	#save_fig(figname, w=9, h=5)
	#ggsave(file = 't.pdf',plot=p2, onefile=F)
	ggsave(figname, plot=last_plot(), width=9, height=5, onefile=F)
	cat(sprintf('\n- file saved to: %s\n\n', figname))
}

#------------------------------------
#
plot_num_authors_per_paper_distribution = function(df) {
    print(df[order(-num_author)])
 
	if (T) {
		ggplot(data = df, aes(x = num_author)) + 
			geom_histogram(fill='#00B6EB', binwidth=0.05, color='white') +
			xlab('Team Size: Number of Authors in a Paper (Log Scale)') + ylab('Frequency (Log Scale)') + 
			scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,20,30,50,100,200,500), 
				trans="log1p", expand=c(0.01,0.01))  +
			scale_y_continuous(breaks=c(0,1,10,100,1000,10000), trans="log1p", expand=c(0.01,0.01)) +
    		theme( panel.background = element_blank()) +
		theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"))
	} else {
		ggplot(data=df, aes(x = num_author)) +
  		geom_histogram(binwidth = .05, fill = "lightgray", color = "red") +
  		scale_x_log10() +
  		labs(title = "Log-Scale Distribution of Co-Authors", 
			x = "Number of Co-Authors (Log Scale)", y = "Frequency")
	}

	fpath_output = create_filename_for_output(sprintf('num_authors_per_paper'), 'pdf')
	save_fig(fpath_output, w=6, h=3)
}


#------------------------------------
#
plot_num_papers_per_author_distribution = function(df) {
	df[, first_author_paper_cnt:=.N, by = list(author_id_first)]
	df[, last_author_paper_cnt:=.N, by = list(author_id_last)]

	print(df[1,]$papers_first)

	dt_first = unique(df[, .(author_id_first, first_author_paper_cnt)])
	dt_last = unique(df[, .(author_id_last, last_author_paper_cnt)])
	colnames(dt_first) = c('first_or_last_author', 'num_of_papers')
	dt_first$first_or_last_author = 'As First Author'
	colnames(dt_last) = c('first_or_last_author', 'num_of_papers')
	dt_last$first_or_last_author = 'As Last Author'
	dt = bind_rows(dt_first, dt_last)

	ggplot(data=dt, aes(num_of_papers, fill=first_or_last_author)) +
		geom_histogram(bins=15, binwidth=0.08, position="dodge", color='white') + 
		xlab('Number of Obs. Studies Published as First or Last Author (Log Scale)') +
		ylab('Frequency (Log Scale)') +
		scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,15,20,30,50,100,200), trans="log1p", expand=c(0.01,0.01))  +
		scale_y_continuous(breaks=c(1,10,100,1000,10000,50000), trans="log1p", expand=c(0.01,0.01))  +
		theme(legend.position = c(0.85, 0.5), legend.title = element_blank()) +
      	theme( panel.background = element_blank()) +
		theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"))

	fpath_output = create_filename_for_output(sprintf('num_papers_per_author'), 'pdf')
	save_fig(fpath_output, w=6, h=3)
	quit()

	for (position in c('first', 'last')) {
		x = sprintf('%s_author_paper_cnt', position)
		plt = ggplot(data=df, aes_string(x=x)) + geom_histogram() + 
			xlab(sprintf('Number of papers published as %s author', position)) +
			ylab('Frequency') +
			scale_x_continuous(breaks=c(0,1,2,3,4,5,10,20,30,50,100,200), trans="log1p", expand=c(0,0)) 
		fpath_output = create_filename_for_output(sprintf('num_papers_per_%s_author', position), 'pdf')
		save_fig_in_loop(fpath_output, plt, w=8, h=3.5)
	}
}

