setwd("~/Projects/trump_voters_stimulus")

state_abbrevations = read_csv('data/state_abbreviations.csv')

arc_counties = read_csv('data/arc_counties.csv') %>%
  mutate(fips=str_trim(fips)) %>%
  mutate(state_fips=str_sub(fips, 1, 2), county_fips=str_sub(fips, 3, 5)) %>%
  mutate(state=cdlTools::fips(state_fips, to='Abbreviation')) %>% # get state abbreviation to make life easier
  mutate(postal_fips=paste(state, county_fips, sep="")) # this will make our life easier later, with big L2 file

county_election_results = read_csv('data/potus_2020_county.csv') %>%
  filter(`Geographic Subtype` == 'County') %>% # there is an additional header row that we drop
  select(FIPS, `Total Vote`, `Joseph R. Biden Jr.`, `Donald J. Trump`) %>%
  rename(total_vote=`Total Vote`, biden_vote=`Joseph R. Biden Jr.`, trump_vote=`Donald J. Trump`, county_fips=FIPS) %>%
  mutate(total_vote=as.integer(total_vote), trump_vote=as.integer(trump_vote), biden_vote=as.integer(biden_vote)) %>%
  mutate(county_fips=str_pad(county_fips, 5, pad='0')) %>%
  mutate(trump_vote_frac=trump_vote/total_vote, biden_vote_frac=biden_vote/total_vote)

state_election_results = county_election_results %>%
  mutate(state_fips=str_sub(county_fips, 1, 2)) %>%
  group_by(state_fips) %>%
  summarize(total_vote=sum(total_vote),
            biden_vote=sum(biden_vote),
            trump_vote=sum(trump_vote),
            .groups='drop') %>%
  mutate(trump_vote_frac=trump_vote/total_vote, biden_vote_frac=biden_vote/total_vote)

additional_state_data = read_csv('data/additional_state_data.csv') %>%
  select(state_fips, z.average_income, pvi, region)