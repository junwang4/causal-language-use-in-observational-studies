source("./R_regression_analysis_helpers.R")
load_packages()

library("argparse")
parser <- ArgumentParser()
parser$add_argument("--add_num_of_authors", default='T')
parser$add_argument("--add_author_paper_cnt", default='T')
parser$add_argument("--add_journal_paper_cnt", default='T')
parser$add_argument("--add_sjr", default='T')
parser$add_argument("--add_gender", default='T')
parser$add_argument("--add_country", default='T')
parser$add_argument("--add_mesh_terms", default='T')
parser$add_argument("--add_journal", default='T')

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

cat('\n')

tic()

fpath_data = 'csv/data_main.csv'
fpath_mesh_term = 'csv/data_meshterm.csv'

result = load_and_preprocess_data(fpath_data, fpath_mesh_term)
df = result[[1]]
factor_of_mesh_terms = result[[2]]

model_overall = run_regression_model(df,
                                     factor_of_mesh_terms,
									 add_journal = args$add_journal,
									 add_mesh_terms = args$add_mesh_terms,
									 add_author_paper_cnt = args$add_author_paper_cnt,
									 add_journal_paper_cnt = args$add_journal_paper_cnt,
									 add_sjr = args$add_sjr,
									 add_num_of_authors = args$add_num_of_authors,
									 add_gender = args$add_gender,
									 add_country = args$add_country)

toc()

folder_model_output = 'working'
save_regression_model(model_overall,  args$add_journal, args$add_mesh_terms, folder_model_output)

cat(sprintf('\n==END\n- data source = %s\n\n\n', fpath_data))
toc()