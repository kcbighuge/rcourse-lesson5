library(dplyr)
library(purrr)

## READ IN DATA ##
data_elec_results = list.files(path='data/elections/', full.names=T) %>%
    map(read.table, header=T, sep='\t') %>%
    reduce(rbind)

str(data_elec_results)
summary(data_elec_results)

# Read in extra data about elections & states
data_elec = read.table('data/rcourse_lesson5_data_elections.txt', header=T, sep='\t')
data_states = read.table('data/rcourse_lesson5_data_states.txt', header=T, sep='\t')

# look at states in union vs confederacy
xtabs(~civil_war, data_states)

## CLEAN DATA ##
# balance the dataset for union & confederacy
data_states_clean = data_states %>%
    filter(!is.na(civil_war)) %>%
    group_by(civil_war) %>%
    arrange(order_enter) %>%
    filter(row_number() <= 11) %>%
    ungroup()

# check for balanced variable
xtabs(~civil_war, data_states_clean)

# combine 3 df's
data_clean = data_elec_results %>%
    inner_join(data_elec) %>%
    inner_join(data_states_clean) %>%
    mutate(state=factor(state))

# check for balanced numbers
xtabs(~incumbent_party+civil_war, data_clean)
