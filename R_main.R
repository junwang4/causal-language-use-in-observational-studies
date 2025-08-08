source("./R_helpers.R")
source("./R_plot.R")
source("./R_regression_analysis.R")
load_packages()

data_dir = 'csv'
model_output_dir = 'working/model'

#library("argparse")
parser <- ArgumentParser()
parser$add_argument("--task", default='')
parser$add_argument("--plot_task", default='')
parser$add_argument("--add_num_of_authors", default='T')
parser$add_argument("--add_author_paper_cnt", default='T')
parser$add_argument("--add_journal_paper_cnt", default='T')
parser$add_argument("--add_sjr", default='T')
parser$add_argument("--add_gender", default='T')
parser$add_argument("--add_country", default='T')
parser$add_argument("--add_mesh_terms", default='T')
parser$add_argument("--add_journal", default='T')
parser$add_argument("--meshterm_min_paper_cnt", default=150, type="integer")
parser$add_argument("--gender_min_confidences", default='M80_F80')

args <- parser$parse_args()

cat('\n')

args$add_gender = args$add_gender == 'T'
args$add_country = args$add_country == 'T'
args$add_num_of_authors = args$add_num_of_authors == 'T'
args$add_sjr = args$add_sjr == 'T'
args$add_author_paper_cnt = args$add_author_paper_cnt == 'T'
args$add_journal_paper_cnt = args$add_journal_paper_cnt == 'T'
args$add_journal = args$add_journal == 'T'
args$add_mesh_terms = args$add_mesh_terms == 'T'

cat(sprintf('- add_journal = %s\n', args$add_journal))
cat(sprintf('- add_mesh_terms = %s\n', args$add_mesh_terms))

cat(sprintf('- add_num_of_authors = %s\n', args$add_num_of_authors ))
cat(sprintf('- add_gender = %s\n', args$add_gender))
cat(sprintf('- add_country = %s\n', args$add_country))
cat(sprintf('- add_author_paper_cnt = %s\n', args$add_author_paper_cnt))
cat(sprintf('- add_journal_paper_cnt = %s\n', args$add_journal_paper_cnt))
cat(sprintf('- add_sjr = %s\n', args$add_sjr))

meshtermTH = args$meshterm_min_paper_cnt
genderTH = args$gender_min_confidences

cat(sprintf('- meshterm_min_paper_cnt = %s\n', meshtermTH))
cat(sprintf('- gender_min_confidences = %s\n', genderTH))
cat('\n')

fpath_data = sprintf('%s/data_main.csv', data_dir)
fpath_mesh_term = sprintf('%s/data_meshterm.csv', data_dir)
fpath_UAI = sprintf('%s/UAI.csv', data_dir)

model_no = sprintf('journal%s_meshterm%s_meshterm%s_gender%s', args$add_journal, args$add_mesh_terms, meshtermTH, genderTH)
model_no = gsub("TRUE", "T", gsub("FALSE", "F", model_no))
fpath_model_coeff = create_filename_for_output(sprintf('coef_%s', model_no), "csv", folder=model_output_dir)
fpath_model_param = create_filename_for_output(sprintf('param_%s', model_no), "csv", folder=model_output_dir)
cat(sprintf('- model coefficients file: %s\n', fpath_model_coeff))

if (args$task == 'run_regression_analysis') {
	tic()
	result = load_and_preprocess_data(fpath_data, fpath_mesh_term, args$add_mesh_terms, meshtermTH, genderTH)
	df = result[[1]]
	meshterm_list_as_formula = result[[2]]

	model_overall = run_regression_model(df,
                                     meshterm_list_as_formula,
									 add_journal = args$add_journal,
									 add_mesh_terms = args$add_mesh_terms,
									 add_author_paper_cnt = args$add_author_paper_cnt,
									 add_journal_paper_cnt = args$add_journal_paper_cnt,
									 add_sjr = args$add_sjr,
									 add_num_of_authors = args$add_num_of_authors,
									 add_gender = args$add_gender,
									 add_country = args$add_country,
									 )
	toc()
	save_regression_model(model_overall, fpath_model_coeff, fpath_model_param)

} else if (args$task == 'plot') {
	df_main = fread(fpath_data)

	plot_task = args$plot_task
	cat(sprintf('- plot_task = %s\n', plot_task))

	if (plot_task == 'country_UAI_vs_causal_language_use') {
		plot_country_uncertainty_avoidance_index_vs_causal_language_use(fpath_UAI, fpath_data, fpath_model_coeff)

	} else if (plot_task == 'gender_distribution_by_author_position') {
		plot_gender_distribution_by_author_position(fpath_data)

	} else if (plot_task == 'y_distribution') {
    	plot_y_distribution(df_main)

	} else if (plot_task == 'country_distribution') {
    	plot_country_distribution(df_main)

	} else if (plot_task == 'team_size_distribution') {
		plot_num_authors_per_paper_distribution(df_main)

	} else if (plot_task == 'author_experience_distribution') {
		plot_num_papers_per_author_distribution(df_main)
	}
}
