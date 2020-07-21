# Misc

Collection of miscellaneous functions

# gower_dist.py
Python implementation of gower distance based dissimilarity matrix

# diff_in_diff_power_calc_percs.R
Function to perform power analysis for difference in difference tests based on simulation/boostrapping. Estiamte power of your test given expected lift (via pre/post test/control percentages) and sample sizes. Contrary to a/b testing power analysis scenario where you input test/control percentages, significance level (alpha) & power (beta) and get minimum sample size, this function expects test/control percentages for pre & post, significance level (alpha) & sample sizes that will be supplied and outputs the power (beta) that can be achieved given these inputs. Note: Function is built for power analysis when the metrics under consideration are percentages
