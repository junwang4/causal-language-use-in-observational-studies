# load libraries
for (pkg in c("data.table", "dplyr", "ggplot2", "argparse", "ggrepel")) {
    suppressMessages(require(pkg, character.only = TRUE))
}

df_main = fread('csv/data_main.csv')
dt_model_coef = fread(sprintf('working/coef_size%d_journalT_meshtermT.csv', nrow(df_main)), header = FALSE, col.names = c('var', 'est', 'stderr', 'z', 'pval'))


#------------------------------------
#
my_corr_test = function(dt, msg) {
    cor_test <- cor.test(dt$est, dt$UAI, method = "pearson")
    pearson_corr <- cor_test$estimate
    p_value <- cor_test$p.value
    cat(sprintf("\n%s    Correlation: %.2f    p-value: %.5f", msg, pearson_corr, p_value))
}

create_filename_for_output = function(namebase, ext='pdf', folder='pdf_out') {
	folder_of_results = folder
	ifelse(!dir.exists(folder_of_results), dir.create(folder_of_results, recursive=TRUE), FALSE)
	filename = paste(c(namebase, '.', ext), collapse = '')
	return (paste(c(folder_of_results, filename), collapse = '/') )
}

save_fig = function(figname, w = 5, h = 3) {
	ggsave(figname, plot=last_plot(), width=w, height=h, dpi=300)
	cat(sprintf('\n- file saved to: %s\n\n', figname))
}

#------------------------------------
#
plot_country_uncertainty_avoidance_index_vs_causal_language_use = function() {
    dt_coef <- dt_model_coef[grepl("^countryc_", var), .(est, country_code = substr(var, nchar(var)-1, nchar(var)))]
    dt_uai = fread('csv/UAI.csv')
    dt = merge(dt_coef, dt_uai, by = "country_code", all.x =  TRUE)

    non_western_country_list <- strsplit('CN JP KR TW EG TR IL IN IR BR MX', " ")[[1]]
    dt[, is_western := ifelse(country_code %in% non_western_country_list, F, T)]
    dt_western = dt[!country_code %in% non_western_country_list]

    print(dt[order(-est), .(country, est, UAI, is_western)])

    my_corr_test(dt, 'all    ')
    my_corr_test(dt_western, 'western')
    cat('\n\n')

    # https://htmlcolorcodes.com/color-names/
    p <- ggplot(dt, aes(x = UAI, y = est)) +
        geom_point(aes(shape = is_western, color=is_western, fill=is_western), size = 2.5, stroke=1.3) +
        scale_color_manual(values = c("Salmon", "CornflowerBlue")) +
        scale_shape_manual(values = c(17, 19)) +
        geom_text_repel(aes(label = country), color='#000000', size = 3.3, nudge_x = 0, nudge_y = 0.004)

    p <- p + labs( x = "Uncertainty Avoidance Index", y = "Use of causal claims") + theme(
        legend.position = "none",
        axis.text = element_text(size = 9.5),  # Adjust the size of tick labels
        axis.title = element_text(size = 10),  # Adjust the size of axis labels
        plot.title = element_text(size = 12)   # Adjust the size of the plot title
    )

    p <- p + theme( panel.background = element_blank())
	fpath_output = create_filename_for_output('uncertainty_vs_causal', 'eps')  # arxiv submission failed: missing bounding box for pdf
	save_fig(fpath_output, w=5, h=3.0)
	fpath_output = create_filename_for_output('uncertainty_vs_causal', 'pdf')  # arxiv submission failed: missing bounding box for pdf
	save_fig(fpath_output, w=5, h=3.0)
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

plot_gender_distribution_by_author_position = function() {
    df = df_main
    print(table(df$gender_first))
    print(prop.table(table(df$gender_first)))
    print(sum(df$gender_first == "L") / sum(df$gender_first %in% c("F", "M", "L")))
    print(sum(df$gender_first == "F") / sum(df$gender_first %in% c("F", "M", "L")))
    print(sum(df$gender_first == "M") / sum(df$gender_first %in% c("F", "M", "L")))
    print(sum(df$gender_first %in% c("I", "L")) / sum(df$gender_first %in% c("F", "M", "L", "I")))

    print(table(df$gender_last))
    print(prop.table(table(df$gender_last)))
    print(sum(df$gender_last == "L") / sum(df$gender_last %in% c("F", "M", "L")))
    print(sum(df$gender_last == "F") / sum(df$gender_last %in% c("F", "M", "L")))
    print(sum(df$gender_last == "M") / sum(df$gender_last %in% c("F", "M", "L")))
    print(sum(df$gender_last %in% c("I", "L")) / sum(df$gender_last %in% c("F", "M", "L", "I")))

	dt = melt(df[, .(pmid, gender_first, gender_last)], id='pmid')
	colnames(dt) = c('pmid', 'first_or_last', 'gender')
	dt[, first_or_last:=ifelse(first_or_last=='gender_first', 'first author', 'last author')]

	ggplot(data=dt, aes(x=first_or_last, fill=gender)) + geom_bar(stat="count", position="dodge") +
		scale_fill_manual(values=c("#F8766D", "#b1cbbb", "#f1c232", "#00B6EB")) +
		xlab('') + ylab('') +
		theme(panel.grid = element_blank(),  panel.background = element_blank())

	fpath_output = create_filename_for_output(sprintf('gender_distribution'), 'pdf')
	print(fpath_output)

	save_fig(fpath_output, w=3.5, h=2.5)
}


parser <- ArgumentParser()
parser$add_argument("--task", default='')
args <- parser$parse_args()
task = args$task

if (task == 'plot_gender_distribution_by_author_position') {
    plot_gender_distribution_by_author_position()
} else if (task == 'plot_gender_distribution_by_country') {
    plot_gender_distribution_by_country()
} else if (task == 'plot_country_uncertainty_avoidance_index_vs_causal_language_use') {
    plot_country_uncertainty_avoidance_index_vs_causal_language_use()
} else {
    cat('\n- wrong task:', task, '\n')
}