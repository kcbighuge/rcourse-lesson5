## READ IN DATA ##
source('rcourse5_cleaning.R')

## LOAD PACKAGES ##
library(ggplot2)
rm(list=c('data_elec','data_elec_results','data_states','data_states_clean'))

## ORGANIZE DATA ##
data_figs = data_clean %>%
    mutate(civil_war = factor(civil_war, 
                              levels = c('union', 'confederacy'),
                              labels = c('Union', 'Rebs'))) %>%
    mutate(incumbent_party = factor(incumbent_party,
                                    levels = c('democrat', 'republican'),
                                    labels = c('Dems', 'Repubs')))

# Average data over years but not states
data_figs_state_sum = data_figs %>%
    group_by(state, incumbent_party, civil_war) %>%
    summarise(perc_incumbent_mean = 
                  mean(perc_votes_incumbent, na.rm=T)) %>%
    ungroup

## MAKE FIGS ##
# histogram of full data
incumbent_hist_full.plot = ggplot(data_figs, aes(x=perc_votes_incumbent,
                                                 fill=incumbent_party)) +
    geom_histogram(bins=10) +
    facet_grid(incumbent_party ~ civil_war) + 
    scale_fill_manual(values = c('blue', 'red'))
# save plot as pdf
pdf('figures/incumbent_hist_full.pdf')
incumbent_hist_full.plot
dev.off()

# histogram of data averaged over years
incumbent_hist_sum.plot = ggplot(data_figs_state_sum, 
                                 aes(x=perc_incumbent_mean,
                                     fill=incumbent_party)) +
    geom_histogram(bins=10) +
    facet_grid(incumbent_party ~ civil_war) + 
    scale_fill_manual(values = c('blue', 'red'))
incumbent_hist_sum.plot
# save plot as pdf
pdf('figures/incumbent_hist_sum.pdf')
incumbent_hist_sum.plot
dev.off()

## Boxplot ##
incumb_box.plot = ggplot(data_figs_state_sum,
                         aes(x=civil_war, y=perc_incumbent_mean,
                              fill=incumbent_party)) +
    geom_boxplot() +
    ylim(30,70) +
    geom_hline(yintercept = 50) +
    scale_fill_manual(values = c("blue", "red"))
incumb_box.plot
# save plot as pdf
pdf("figures/incumbent_box.pdf")
incumb_box.plot
dev.off()









