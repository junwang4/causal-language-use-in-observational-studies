## Introduction

This repository contains the code for the
paper `Causal Claims in Observational Studies: How Sociocultural Backgrounds 
and Team Dynamics Influence Causal Interpretations`.
The code is written in R and uses the `lme4` package to fit the
logistic linear mixed-effects regression model.

## Running the code
1. Clone the repository
2. Package dependencies

```
packages_to_install <- c("argparse", "ggrepel", "data.table", "tidyverse", "tictoc", "lme4")
install.packages(packages_to_install)
```

3. Run the regression analysis
```
Rscript R_regression_analysis_main.R
```

4. Plot the country uncertainty avoidance index versus the country effects of causal language use.
```
Rscript R_plot.R --task=plot_country_uncertainty_avoidance_index_vs_causal_language_use
```

5. Plot the gender distribution by country and author position.
```
Rscript R_plot.R --task=plot_gender_distribution_by_country
Rscript R_plot.R --task=plot_gender_distribution_by_author_position
```

## Issues

When running `Rscript R_regression_analysis_main.R`, you may get the following error:
```
Error: C stack usage 7976892 is too close to the limit
```

Answer: Running the following on your terminal before running the regression analysis code:
```
ulimit -s unlimited
```
