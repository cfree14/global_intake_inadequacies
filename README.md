# Estimating global nutrient intake adequacy

This GitHub repository contains the data and code for the following paper in press:

* Passarelli S, Free CM,  Shepon A, Beal T, Batis C, Golden CD (in press) Global estimation of dietary micronutrient inadequacies: a modeling analysis. In press at _The Lancet Global Health_.

## R Shiny web application

An R Shiny web application to explore global nutrient intake inadequacies is available here: https://emlab-ucsb.shinyapps.io/global_intake_inaqequacies/

## Repository structure

The repository is organized using the following structure:

* data.............. folder containing input data for the analysis
* code............. folder containing code to conduction the analysis
* figures.......... folder containing figures for the manuscript
* output........... folder containing output generated in the analysis
* tables........... folder containing tables for the manuscript
* shiny_app..... folder containing data and code for the "nutriR" R Shiny web application

The data files containing the final subnational prevalence of inadequate nutrient intake estimates is located here:

* output/2018_subnational_nutrient_intake_inadequacy_estimates_simple.csv (for anyone)
* output/2018_subnational_nutrient_intake_inadequacy_estimates_simple.Rds (for R progammers)

The data files containing these values summarized at the national level are located here:

* output/2018_national_nutrient_intake_inadequacy_estimates.csv (for anyone)
* output/2018_national_nutrient_intake_inadequacy_estimates.Rds (for R progammers)

Please email Simone Passarelli (simoneapassarelli@gmail.com) if you have any questions about the paper and Chris Free (cfree14@gmail.com) if you have any questions about the data, code, and/or repository.

## Related resources

The paper leans heaviliy on data and code developed by Passarelli et al. (2021) and the associated nutriR R package. Links to the paper, the nutriR package, and the nutriR package vignette are provided below:

1. Passarelli S, Free CM, Allen LH, Batis C, Beal T, Biltoft-Jensen AP, Bromage S, Cao L, Castellanos-Guitiérrez A, Christensen T, Crispim SP, Dekkers A, De Ridder K, Kronsteiner-Gicevic S, Lee C, Li Y, Moursi M, Moyersoen I, Schmidhuber J, Shepon A, Viana DF, Golden CD (2022) Estimating national and sub-national habitual nutrient intake distributions of global diets. _The American Journal of Clinical Nutrition_ 116(2): 551-560. [[GitHub repository]](https://github.com/cfree14/subnational_nutrient_distributions/) [[link]](https://academic.oup.com/ajcn/article/116/2/551/6605334)
2. Free CM, Passarelli S, Allen LH, Batis C, Beal T, Biltoft-Jensen AP, Bromage S, Cao L, Castellanos-Guitiérrez A, Christensen T, Crispim SP, Dekkers A, De Ridder K, Kronsteiner-Gicevic S, Lee C, Li Y, Moursi M, Moyersoen I, Schmidhuber J, Shepon A, Viana DF, Golden CD (2021) nutriR: Nutritional intake functions for R. Available at: https://github.com/cfree14/nutriR
3. A vignette illustrating the functionality of the "nutriR" package is available here: https://chrismfree.com/wp-content/uploads/nutriR-vignette.html
