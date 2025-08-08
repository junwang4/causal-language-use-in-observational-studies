# NOTE: on linux
#   in case of "Error: C stack usage 7976892 is too close to the limit"
#   run the following on your terminal before running the R code:
#	    ulimit -s unlimited

add_num_of_authors = T
add_author_paper_cnt = T
add_gender = T
add_country = T

add_journal_paper_cnt = T
add_sjr = T

add_mesh_terms = T
add_journal = T

#meshterm_min_paper_cnt = 1000
meshterm_min_paper_cnt = 100

# M82/F78: male/female confidence cutoffs, corresponding to 0.125 rejection rate (that is, 12.5% of names will be classified as Unknown) on a PeerJ benchmark dataset
#gender_min_confidences = M80_F80   # as a baseline reference
gender_min_confidences = M82_F78   # 0.125 rejection rate


#quick: regression
#quick: tables
#quick: plots
quick: y_distribution


regression:
	Rscript R_main.R \
	--task=run_regression_analysis \
	--add_num_of_authors=$(add_num_of_authors) \
	--add_author_paper_cnt=$(add_author_paper_cnt) \
	--add_journal_paper_cnt=$(add_journal_paper_cnt) \
	--add_sjr=$(add_sjr) \
	--add_gender=$(add_gender) \
	--add_country=$(add_country) \
	--add_mesh_terms=$(add_mesh_terms) \
	--add_journal=$(add_journal) \
	--meshterm_min_paper_cnt=$(meshterm_min_paper_cnt) \
	--gender_min_confidences=$(gender_min_confidences)


PLOT_TASKS = \
    y_distribution \
    gender_distribution_by_author_position \
    country_distribution \
    author_experience_distribution \
    team_size_distribution \
    country_UAI_vs_causal_language

plots: $(PLOT_TASKS)

$(PLOT_TASKS):
	Rscript R_main.R \
	--task=plot \
	--plot_task=$@ \
	--meshterm_min_paper_cnt=$(meshterm_min_paper_cnt) \
	--gender_min_confidences=$(gender_min_confidences)


TABLE_TASKS = \
	meshterm_effect \
	flowchart_data_collection \
	overall_effect

tables: $(TABLE_TASKS)

$(TABLE_TASKS):
	python py_charts_and_tables.py \
		--task=$@ \
		--meshterm_min_paper_cnt=$(meshterm_min_paper_cnt) \
		--gender_min_confidences=$(gender_min_confidences)