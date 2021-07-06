library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(usmap)
library(ggplot2)

source('base_data.R')

# read trump vote probabilities
p_trump = read_csv('outputs/county_model_1_appalachia_voters_p_trump_corrected.csv')

# similar data as in 02, except that it only contains household id + voterid
df = read_csv('data/voterfile_appalachia_household_data.csv') %>%
  mutate(postal_fips=paste(state, voters_fips, sep="")) %>%
  #filter(postal_fips %in% arc_counties$postal_fips) %>%
  mutate(voted=ifelse(general_2020 == 'Y', 1, 0)) %>%
  tidylog::left_join(p_trump, by='lalvoterid') %>%
  replace_na(list(p_voted_trump=0, p_voted_trump_corrected=0)) # if they didn't get, they get a trump probability of zero


df_by_household = df %>%
  group_by(state, residence_families_householdid) %>% # group by household ids
  summarize(n_voted_in_household=sum(voted, na.rm = TRUE),
            hh_income=first(commercialdata_estimatedhhincome), 
            hh_size=first(residence_families_hhcount),
            county_fips=first(voters_fips),
            # n trump voters per household in poission binomial, with expected value sum of probabilities
            expected_n_trump_voters = sum(p_voted_trump),
            expected_n_trump_voters_corrected = sum(p_voted_trump_corrected), 
            .groups='drop')


df_by_household_w_stimulus = df_by_household %>%
  drop_na(hh_income) %>%
  separate(hh_income, sep='-', into=c(NA, 'max_income')) %>% # parse income, extract maximum
  replace_na(list(max_income=Inf)) %>% # for households where income is 250000+
  mutate(max_income=as.double(max_income)) %>%
  mutate(stimulus_eligible=ifelse(
    # if household size is less than 1, then stimulus eligible if income is less than 75000
    # if household size is less than 2, then stimulus eligible if income is less than 150000
    (hh_size < 2 & max_income < 75000) | (hh_size > 1 & max_income < 150000), 1, 0
    )
  )

# expected number of trump voters * stimulus eligibility: total number of trump voters that are stimulus eligible
print(df_by_household_w_stimulus$stimulus_eligible %*% df_by_household_w_stimulus$expected_n_trump_voters_corrected)

# number of total trump voters according to results
print(county_election_results %>% filter(county_fips %in% arc_counties$fips) %>% filter(str_sub(county_fips, 1, 2) != '21') %>% summarize(sum(trump_vote)))
# number of trump voters according to model: this should be lower since we drop people (!) and also l2 number is slightly lower in general
print(df_by_household_w_stimulus %>% summarize(sum(expected_n_trump_voters_corrected)))

# get county totals to plot and find interesting places
df_w_frac_stimulus_eligible = df_by_household_w_stimulus %>% 
  group_by(state, county_fips) %>%
  # percent of trump voters that are stimulus elgible
  summarize(percent_trump_vote_eligible=sum(stimulus_eligible * expected_n_trump_voters) / sum(expected_n_trump_voters),
            percent_trump_vote_eligible_corrected=sum(stimulus_eligible * expected_n_trump_voters_corrected) / sum(expected_n_trump_voters_corrected),
            l2_voters=sum(n_voted_in_household, na.rm=TRUE),
            expected_trump_voters=sum(expected_n_trump_voters), 
            expected_trump_voters_corrected=sum(expected_n_trump_voters_corrected),
            .groups='drop') %>%
  mutate(state_fips=str_pad(usmap::fips(state), 2, pad="0")) %>%
  mutate(fips=paste(state_fips, county_fips, sep="")) %>%
  tidylog::left_join(arc_counties, by=c('fips', 'state', 'county_fips')) %>%
  tidylog::left_join(county_election_results, by=c('fips'='county_fips')) %>%
  select(state, county, fips, percent_trump_vote_eligible, percent_trump_vote_eligible_corrected, l2_voters, total_vote, trump_vote, expected_trump_voters, expected_trump_voters_corrected) %>%
  mutate(l2_frac=expected_trump_voters / l2_voters, l2_frac_corrected=expected_trump_voters_corrected / l2_voters, actual_frac=trump_vote / total_vote)
  
df_w_frac_stimulus_eligible_to_save = df_w_frac_stimulus_eligible %>%
  select(state, fips, county, percent_trump_vote_eligible_corrected, total_vote, actual_frac) %>%
  rename(trump_frac=actual_frac)
write.csv(df_w_frac_stimulus_eligible_to_save, 'outputs/counties_result_model.csv', row.names=FALSE)
  
plot_usmap(data=df_w_frac_stimulus_eligible, values='percent_trump_vote_eligible_corrected', color=NA, include=c('AL', 'MS', 'GA', 'FL', 'OH', 'VA', 'PA', 'NC', 'NY', 'KY', 'WV', 'TN', 'SC')) +
    scale_fill_continuous(name = "% Trump voters eligible", type='viridis', na.value='grey95') +
    theme(legend.position = "right")
