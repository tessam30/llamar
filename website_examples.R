library(dplyr)
library(haven)
library(RColorBrewer)


# data sets ---------------------------------------------------------------

dhs_orig = read_dta('~/Documents/USAID/Rwanda/processeddata/DHS_2010_2015_analysis.dta')
dhs = removeAttributes(dhs_orig) %>% 
  filter(year == 2014)

dhs = factorize(dhs, dhs_orig, 'lvdzone', 'lz')
df2 = data.frame(avg = sample(-100:100, 10)/100, region = letters[1:10], ci = sample(1:100, 20)/1000) %>% mutate(lb = avg - ci, ub = avg + ci)


# plot_dot ----------------------------------------------------------------
plot_avg_dot(dhs, value_var = 'stunted2', by_var = 'lz', 
              percent_vals = TRUE, weight_var = 'cweight',
             dot_size = 9, 
             dot_fill_cont = rev(brewer.pal(11, 'Spectral')[1:6]), sat_threshold = 0.65) +
  theme_stroke()

save_plot('~/GitHub/llamar/img/plot_dot1.png', width = 8, height = 8)

plot_dot(df2, by_var = 'region', value_var = 'avg', ref_line = 0, 
         ref_text = 'no change', label_ref = FALSE, lollipop = TRUE, value_label_offset = .125,
         dot_fill_cont = brewer.pal(10, 'RdYlBu'), percent_vals = TRUE) +
  theme_stroke()
save_plot('~/GitHub/llamar/img/plot_dot2.png', width = 5, height = 5)

calcPtEst(dhs, var = 'stunted', by_var = 'lz', use_weights = T, weight_var = 'cweight', psu_var = 'psu', strata_var = 'strata')

z=plot_dot(x, value_var = 'avg', by_var = 'lz', plot_ci=T,
# ref_line = TRUE, percent_vals = TRUE, 
             dot_size = 9, 
             dot_fill_cont = rev(brewer.pal(11, 'Spectral')[1:6]))

plot_n(x, 'lz', incl_y_labels = TRUE)
