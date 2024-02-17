# MC-testing-by-betting

This repository contains the R code for reproducing the results in the paper "Sequential Monte-Carlo testing by betting" (https://arxiv.org/abs/2401.07365). In the aforementioned paper, we introduced an anytime-valid Monte-Carlo test, where the observed data is fixed and additional data is generated one by one over time such that computational resources can be saved by stopping early if the evidence for or against the null hypothesis is strong. 

Files:

Power_nperm_generate_data.R: Calculates the power and number of permutations required for reproducing the plots in Figures 2,3,4,5,7 and saves them in the result folder
                             (files "power_alpha005.rda" and "power_alpha001.rda").

