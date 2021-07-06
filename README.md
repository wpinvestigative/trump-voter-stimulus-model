# trump-voter-stimulus-model

Model to estimate the fraction of Trump voters in Appalachia that received stimulus money.

## Data
Necessary data:

1. `CCES20_Common_OUTPUT.csv` - 2020 CES (formerly CCES) results. Can be found [here](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910/DVN/E9N6PH).
2. `potus_2020_county.csv` - 2020 Presidential Election results (at county level). As published by the MIT Election Data + Science Lab, can be found [here](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/42MVDX).
3. `additional_state_data.csv` - region and average income (normalized) is taken from census. State PVI is taken from wikipedia.
4. `arc_counties.csv` - List of counties belonging to Appalachia, as defined by the [Appalachian Regional Commission](https://www.arc.gov/).
5. `voterfile_appalachia_individual_data.csv` - individual level voterfile information from every registered voter in Appalachia (from L2). Including gender, age and ethnicity, as well as state and county fips. Also whether they voted in 2020.
6. `voterfile_appalachia_household_data.csv` - Household level voterfile information for every registered voter in Appalachia (from L2). Including household income, household size, state and county fips.

## Running
1. `01_model_trump_vote.R` - generate model to estimate individual level likelihood of supporting Donald Trump based on 2020 CES.
2. `02_apply_model_to_voterfile.R` - apply the model the voterfile
3. `03_stimulus_in_trump_areas_voterfile.R` - uses model probabilities to compute estimated number of Trump voters that received stimulus (based on L2 income).