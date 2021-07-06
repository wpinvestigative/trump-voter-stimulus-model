library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(rstanarm)
library(stats)

setwd("~/Projects/trump_voters_stimulus")

source('stats_functions.R')
source('base_data.R')

county_election_results = county_election_results %>%
  select(county_fips, biden_vote_frac, trump_vote_frac) %>%
  rename(biden_vote_frac_county=biden_vote_frac, trump_vote_frac_county=trump_vote_frac)

state_election_results = state_election_results %>%
  select(state_fips, biden_vote_frac, trump_vote_frac) %>%
  rename(biden_vote_frac_state=biden_vote_frac, trump_vote_frac_state=trump_vote_frac)

df_voters = read_csv('data/voterfile_appalachia_individual_data.csv') %>%
  mutate(postal_fips=paste(state, voters_fips, sep="")) %>% # we generate this to make the next step easier, otherwise we would need to lookup fips for ~60m
  #filter(postal_fips %in% arc_counties$postal_fips) %>% # originally ~60m voters (thanks NY and Ohio), but only keep the ones in Appalachia
  filter(general_2020 == 'Y') %>% # only keep the ones who voted
  rename(postal_code=state) %>%
  mutate(state_fips=usmap::fips(postal_code)) %>%
  mutate(county_fips=paste(state_fips, voters_fips, sep="")) %>%
  mutate(gender=as.numeric(voters_gender == 'F') + 1) %>% # 1 is male, 2 is female -> from CES
  mutate(gender=if_else(is.na(gender), 9, gender)) %>% # 9 is not asked from CES, could also use 8, which is skipped
  mutate(age_bucket=cut(voters_age, breaks=c(18, 29, 44, 65, Inf))) %>%
  mutate(race=case_when(EthnicGroups_EthnicGroup1Desc == 'European' ~ 1, # convert to same (or similar) from CES
                        EthnicGroups_EthnicGroup1Desc == 'Likely African-American' ~ 2,
                        EthnicGroups_EthnicGroup1Desc == 'Hispanic or Portuguese' ~ 3,
                        EthnicGroups_EthnicGroup1Desc == 'East and South Asian' ~ 4,
                        EthnicGroups_EthnicGroup1Desc == 'Other' ~ 7)) %>%
  mutate(race=if_else(is.na(race), 8, race)) %>%
  select(lalvoterid, state_fips, county_fips, gender, age_bucket, race, general_2020) %>%
  tidylog::left_join(additional_state_data, by='state_fips') %>%
  tidylog::left_join(county_election_results, by='county_fips') %>%
  tidylog::left_join(state_election_results, by='state_fips') %>% 
  drop_na(age_bucket) # need to drop where this is true, since stan prediction doesn't work otherwise

load('models/county_model_1.rda')

# stratify data to make applying the model feasible (or a lot faster)
voters_poststrat = df_voters %>%
  count(race, age_bucket, gender, pvi, z.average_income, biden_vote_frac_state, biden_vote_frac_county, state_fips, region, county_fips)
# sample from posterior, samples 4000 times.
posterior_prob = posterior_linpred(model, transform=TRUE, newdata=voters_poststrat)
# take mean of posterior samples
voters_poststrat['p_voted_trump'] = colMeans(posterior_prob)

# since the predictions came out of a model, there is no reason to think that the probabilities should sum up
# to what we want them to be (ie. the state or county totals)
# To force this to be true, we adjust the strata probabilities to make sure that they sum up to the 
# state/county fraction of Trump/Biden voters.
# If the smallest geographic unit used in the model is state, then the state election results should be passed
# If the smallest geographic unit used in the model is county, then the county election results should be passed
p_voted_trump_corrected = correct_probs(voters_poststrat %>% mutate(fips=county_fips), 
                                        'n', 
                                        'p_voted_trump', 
                                        county_election_results %>% mutate(fips=county_fips), 
                                        'trump_vote_frac_county')
voters_poststrat['p_voted_trump_corrected'] = p_voted_trump_corrected

# join the probabilities to the voters based on their strata
df_voters_min = df_voters %>%
  left_join(voters_poststrat, by=c('race', 'age_bucket', 'gender', 'pvi', 'z.average_income', 'biden_vote_frac_county', 'biden_vote_frac_state', 'state_fips', 'region', 'county_fips')) %>%
  select(lalvoterid, p_voted_trump, p_voted_trump_corrected)
write.csv(df_voters_min, 'outputs/county_model_1_appalachia_voters_p_trump_corrected.csv', row.names=FALSE)
