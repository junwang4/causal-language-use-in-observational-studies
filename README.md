## Introduction

This repository contains the code for the
paper `Causal Language in Observational Studies: Sociocultural Backgrounds and Team Composition`.
The main code is written in R and uses the `lme4` package to fit the
logistic linear mixed-effects regression model.

## Running the code
### 1. Clone the repository
```
git clone https://github.com/junwang4/causal-language-use-in-observational-studies.git
```

### 2. Package dependencies

```
packages_to_install <- c("argparse", "ggrepel", "data.table", "tidyverse", "tictoc", "lme4", "stringr", "ggplot2", "ggbreak", "dplyr", "tidyr")
install.packages(packages_to_install)
```

### 3. Run the regression analysis
```
Rscript R_main.R --task=run_regression_analysis
```

To run a regression analysis with custom parameters, use the following command
```
Rscript R_main.R --task=run_regression_analysis --meshterm_min_paper_cnt=100 --gender_min_confidences=M82_F78
```
Note: M82_F78, as an example, means:
- M82: predict male if name-to-male confidence ≥ 0.82
- F78: predict female if name-to-female confidence ≥ 0.78

### 4. Generate various reports

#### 4.1 Various distributions
(a) Dependent variable (causal or correlational) distribution
```
Rscript R_main.R --task=plot --plot_task=author_experience_distribution
```

(b) Gender distribution 
```
Rscript R_main.R --task=plot --plot_task=gender_distribution_by_author_position
```

(c) Country distribution
```
Rscript R_main.R --task=plot --plot_task=country_distribution
```

(d) Author publication experience distribution
```
Rscript R_main.R --task=plot --plot_task=author_experience_distribution
```

(e) Team size distribution
```
Rscript R_main.R --task=plot --plot_task=team_size_distribution
```


#### 4.2 Country uncertainty avoidance index vs the country effects of causal language use.
```
Rscript R_main.R --task=plot --plot_task=country_UAI_vs_causal_language_use
```

#### 4.3 Estimated effects of author country, gender, authorship position, team size, and writing experience
```
python py_charts_and_tables.py --task=overall_effect --meshterm_min_paper_cnt=100 --gender_min_confidences=M82_F78
```

Effects of 1400+ Mesh terms
```
python py_charts_and_tables.py --task=meshterm_effect --meshterm_min_paper_cnt=100 --gender_min_confidences=M82_F78
```

#### 4.4 Flow chart of the process for collecting observational studies for our analysis.
```
python py_charts_and_tables.py --task=flowchart_data_collection
```


## Issues

When running `Rscript R_main.R --task=run_regression_analysis`, you may get the following error:

```
Error: C stack usage 7976892 is too close to the limit
```

Answer: Running the following on your terminal before running the regression analysis code:
```
ulimit -s unlimited
```
