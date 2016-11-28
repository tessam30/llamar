
library(haven)
dhs_orig = read_dta('~/Documents/USAID/Rwanda/processeddata/DHS_2010_2015_analysis.dta')
dhs = removeAttributes(dhs_orig) %>% 
  filter(year == 2014)

dhs = factorize(dhs, dhs_orig, 'lvdzone', 'lz')

# plot_dot ----------------------------------------------------------------
plot_avg_dot(dhs, value_var = 'stunted', by_var = 'lz', 
             use_weights = TRUE,
             weight_var = 'cweight', ref_line = TRUE, percent_vals = TRUE, 
             dot_fill_cont = rev(brewer.pal(11, 'Spectral')[1:6]))

x=calcPtEst(dhs, var= 'stunted', by_var = 'lz', use_weights = F)

plot_n(x, 'lz', incl_y_labels = TRUE)
