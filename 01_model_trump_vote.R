library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(lme4)
library(rstanarm)
library(ROCR)

source('base_data.R')

county_election_results = county_election_results %>%
  select(county_fips, biden_vote_frac) %>%
  rename(biden_vote_frac_county=biden_vote_frac)

state_election_results = state_election_results %>%
  select(state_fips, biden_vote_frac) %>%
  rename(biden_vote_frac_state=biden_vote_frac)

cces = read_csv('data/CCES20_Common_OUTPUT.csv') %>%
  select(commonpostweight, CC20_401, CC20_410, birthyr, gender, race, inputstate, countyfips) %>%
  filter(CC20_401 == 5) %>% # CC20_401 is whether they voted, CC20_401 == 5 is yes
  mutate(age=2020-birthyr) %>%
  mutate(age_bucket=cut(age, breaks=c(18, 29, 44, 65, Inf))) %>% # bucket ages
  select(-age) %>%
  select(-birthyr) %>%
  mutate(state_fips=str_pad(inputstate, width=2, pad='0')) %>%
  select(-inputstate) %>%
  mutate(county_fips=countyfips) %>%
  select(-countyfips) %>%
  mutate(race=case_when(race == 1 ~ 1, # White
                        race == 2 ~ 2, # Black or African-American
                        race == 3 ~ 3, # Hispanic or Latino
                        race == 4 ~ 4, # Asian or Asian-American
                        race == 5 ~ 7, # Native American, which we change to other because L2
                        race == 6 ~ 7, # Middle Eastern, which we change to other because L2
                        race == 7 ~ 7, # Other
                        race == 8 ~ 7, # Middle Eastern, which we change to other because L2
                        race == 98 ~ 9, # Skipped
                        race == 99 ~ 9)) %>% # not asked 
  filter(CC20_410 == 1 | CC20_410 == 2) %>% # this is who they voted for, 1 is Biden and 2 is Trump
  mutate(voted_trump= as.integer(CC20_410 == 2)) %>% 
  tidylog::left_join(additional_state_data, by='state_fips') %>%
  tidylog::left_join(county_election_results, by='county_fips') %>%
  tidylog::left_join(state_election_results, by='state_fips')

options(mc.cores=4)

train_model = function(train_df, weights=NULL) {
  
  if (is.null(weights)) {
    weights = seq(1, nrow(train_df))
  }
  
  model = stan_glmer(
    voted_trump ~ 
      pvi + z.average_income + 
      (1 + biden_vote_frac_state | state_fips) + (1 + biden_vote_frac_state | region) +
      (1 + biden_vote_frac_county | county_fips) +
      (1 | state_fips:gender) + (1 | state_fips:race) + (1 | state_fips:age_bucket) +
      (1 | region:gender) + (1 | region:race) + (1| region:age_bucket) +
      (1 | gender:race) + (1 | race:age_bucket) + (1 | gender:age_bucket),
    weights=weights,
    family=binomial(link="logit"),
    data=train_df,
    iter=2000
  )
  return(model)
}


# train model on training set and then test on test set 
cces_shuffled = sample(cces) # shuffle data to generate random train test split
last_train_index = floor(0.8 * nrow(cces_shuffled)) # 80% is training set

cces_train = cces[1:last_train_index,]
cces_test = cces[last_train_index:nrow(cces_shuffled), ] %>%
  drop_na() # need to drop na for test, since model can't handle on prediction time

weights_train = cces_train$commonpostweight
weights_test = cces_test$commonpostweight 

model = train_model(cces_train, weights_train)

posterior_sample_test = posterior_predict(model, newdata=cces_test, type="response")
y_hat_test = colMeans(posterior_sample_test)
pred = prediction(y_hat_test, cces_test$voted_trump)
ROC.perf = performance(pred, "tpr", "fpr")
plot(ROC.perf)
auc = performance(pred, "auc")
print(as.numeric(auc@y.values))


# if happy with model retrain on entire ces
weights = cces$commonpostweight
model = train_model(cces, weights)

save(model, file='models/county_model_1.rda')
