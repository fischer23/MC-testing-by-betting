# MC-testing-by-betting

This repository contains the R code for reproducing the results in the paper "Sequential Monte-Carlo testing by betting" (https://arxiv.org/abs/2401.07365). In the aforementioned paper, we introduced an anytime-valid Monte-Carlo test, where the observed data is fixed and additional data is generated one by one over time such that computational resources can be saved by stopping early if the evidence for or against the null hypothesis is strong. 

Files:

Power_nperm_generate_data.R: Calculates the power and number of permutations required for reproducing the plots in Figures 2,3,4,5,7 and saves them in the result folder
                             (files "power_alpha005.rda" and "power_alpha001.rda"). 

Pval_generate_data.R:        Calculates the p-values required for reproducing the plots in Figures 1 and 8 and saves them in the result folder
                             (files "pval_alpha001_mu001.rda", "pval_alpha001_mu01.rda", "pval_alpha001_mu1.rda", "pval_alpha005_mu001.rda", "pval_alpha005_mu01.rda",     
                             "pval_alpha005_mu1.rda").

Plot_generator.R:            Uses the .rda files created by "Power_nperm_generate_data.R" and "Pval_generate_data.R" to generate Figures 1,2,3,4,5,7,8.

Real_data_Fisher_sharp.R:    Real data analysis described in Section 6.4. Generates plots for Figure 6 and saves them in the result folder (files "Plot_wealth_upper.pdf" and                               "plot_wealth_lower.pdf"). The same data was analyzed by Rosenbaum (2002) and Ding (2017).

Real_data_CRT.R:             Real data analysis described in Section 6.5. The part of the code used to generate the test statistics was copied from the supplementary       
                             material of Grünwald et al. (2023). In the code, we explicitly make clear which part was copied.  

Results folder:              Contains the above mentioned .rda files and all plots shown in the paper.

### References for the real data analyses

T. B. Berrett, Y. Wang, R. F. Barber, and R. J. Samworth. The conditional permutation test for independence while controlling for
confounders. Journal of the Royal Statistical Society Series B: Statistical Methodology, 82(1):175-197, 2020.

P. Ding. A paradox from randomization-based causal inference. Statistical Science, pages 331-345, 2017.

P. Grünwald, A. Henzi, and T. Lardy. Anytime-valid tests of conditional independence under model-X. Journal of the American Statistical
Association, pages 1-12, 2023.

P. R. Rosenbaum. Observational Studies. Springer, 2002.



