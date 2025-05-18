# MC-testing-by-betting

This repository contains the R code for reproducing the results in the paper "Sequential Monte-Carlo testing by betting" (https://arxiv.org/abs/2401.07365). In the aforementioned paper, we introduced an anytime-valid Monte-Carlo test, where the observed data is fixed and additional datasets are generated one by one over time such that computational resources can be saved by stopping early if the evidence for or against the null hypothesis is strong. The folder "Multiple testing" contains the R code for reproducing the results of the follow up paper "Multiple testing with anytime-valid Monte-Carlo p-values" (https://arxiv.org/abs/2404.15586).

Files:

power_nperm_generate_data.R: Calculates the power and number of permutations required for reproducing the plots in Figures 1,2,3,4 and saves them in the result folder
                             (files "power_alpha005.rda" and "power_alpha001.rda"). 

plot_generator.R:            Uses the .rda files created by "Power_nperm_generate_data.R" to generate Figures 1,2,3,4.

real_data_fisher_sharp.R:    Real data analysis described in Section 7.3. Generates plots for Figure S.10 and saves them in the result folder (files "Plot_wealth_upper.pdf" and                               "plot_wealth_lower.pdf"). The same data was analyzed by Rosenbaum (2002) and Ding (2017).

real_data_crt.R:             Real data analysis described in Section 7.4. The part of the code used to generate the test statistics was copied from the supplementary material of Grünwald et al. (2023). In the code, we explicitly make clear which part was copied. The dataset is available at 
                             https://ride.capitalbikeshare.com/system-data .

Results folder:              Contains the above mentioned .rda files and all plots shown in the paper.

Appendix folder:             Contains the additional simulations provided in the supplementary material of the paper.

Multiple testing folder:     Contains the code for the follow up paper "Multiple testing with anytime-valid Monte-Carlo p-values".

### References for the real data analyses

T. B. Berrett, Y. Wang, R. F. Barber, and R. J. Samworth. The conditional permutation test for independence while controlling for
confounders. Journal of the Royal Statistical Society Series B: Statistical Methodology, 82(1):175-197, 2020.

P. Ding. A paradox from randomization-based causal inference. Statistical Science, pages 331-345, 2017.

P. Grünwald, A. Henzi, and T. Lardy. Anytime-valid tests of conditional independence under model-X. Journal of the American Statistical
Association, pages 1-12, 2023.

P. R. Rosenbaum. Observational Studies. Springer, 2002.



