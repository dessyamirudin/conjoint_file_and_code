setwd("~/0. Personal/Blog/Conjoint Analysis")

library(tidyverse)
library(readxl)

icecream <- read_excel("icecream.xlsx") # No need to include the sheet argument when there's only one sheet in the Excel file

icecream
icecream <- icecream %>% 
  pivot_longer(cols = starts_with("Individual"), names_to = "respondent", values_to = "rating") %>% # respondent keeps track of the respondent, rating will store the respondent's ratings, and we want to stack every variable that starts with Individual
  rename("profile" = "Observations") %>% # rename Observations to profile
  mutate(profile = factor(profile), respondent = factor(respondent),  # factorize identifiers
         Flavor = factor(Flavor), Packaging = factor(Packaging), Light = factor(Light), Organic = factor(Organic)) # factorize the ice cream attributes


# Wide dataset: one row per unit of observation (here: profile) and a number of columns for the different observations (here: respondents)
# Long dataset: one row per observation (here: profile x respondent combination)

# Converting from wide to long means that we're stacking a number of columns on top of each other.
# The pivot_longer function converts datasets from wide to long and takes three arguments:
# 1. The cols argument: here we tell R which columns we want to stack. The original dataset had 10 rows with 15 columns for 15 individuals. The long dataset will have 150 rows with 150 values for 15 individuals. This means we need to keep track of which individual we're dealing with.
# 2. The names_to argument: here you define the name of the variable that keeps track of which individual we're dealing with.
# 3. The values_to argument: here you define the name of the variable that stores the actual values.

icecream

library(radiant)

# attribute1, attribute2, etc. are vectors with one element in which we first provide the name of the attribute followed by a semi-colon and then provide all the levels of the attributes separated by semi-colons
attribute1 <- "Flavor; Raspberry; Chocolate; Strawberry; Mango; Vanilla"
attribute2 <- "Package; Homemade waffle; Cone; Pint"
attribute3 <- "Light; Low fat; No low fat"
attribute4 <- "Organic; Organic; Not organic"

# now combine these different attributes into one vector with c()
attributes <- c(attribute1, attribute2, attribute3, attribute4)
summary(doe(attributes, seed = 123))
summary(doe(attributes, seed = 123, trials = 30))

respondent1 <- icecream %>% filter(respondent == "Individual 1")

# save the conjoint analysis in an object, because we'll use it as input to summary(), plot(), and predict() later on
conjoint_respondent1 <- conjoint(respondent1, rvar = "rating", evar = c("Flavor","Packaging","Light","Organic")) 

summary(conjoint_respondent1)

plot(conjoint_respondent1)

# predict
profiles <- icecream %>% 
  filter(respondent == "Individual 1") %>% 
  select(Flavor,Packaging,Light,Organic)

profiles

predict(conjoint_respondent1, profiles) # predict the ratings for the profiles based on the conjoint analysis

# new design
Flavor <- c("Raspberry","Chocolate","Mango","Strawberry","Vanilla")
Organic <- c("Organic","Not organic")

expand.grid(Flavor, Organic)

# there's an easier way to get attribute levels than creating the vectors manually:
levels(icecream$Flavor) # make sure that Flavor is factorized first!

# now create all the profiles
profiles.all <- expand.grid(levels(icecream$Flavor),levels(icecream$Packaging),levels(icecream$Light),levels(icecream$Organic)) %>% 
  rename("Flavor" = "Var1", "Packaging" = "Var2", "Light" = "Var3", "Organic" = "Var4") # rename the variables created by expand.grid (don't forget this, otherwise predict won't know where to look for each attribute)

# predict the ratings of all profiles
predict(conjoint_respondent1, profiles.all) %>% 
  arrange(desc(Prediction)) # show the ice creams with the highest predicted rating on top

# all respondent
conjoint_allrespondents <- conjoint(icecream, rvar = "rating", evar = c("Flavor","Packaging","Light","Organic")) # same as before, but different dataset.

summary(conjoint_allrespondents) 

summary(lm(rating ~ Flavor + Packaging + Light + Organic, data = icecream))

# predict all
predict(conjoint_allrespondents, profiles.all) %>% # check previous sections for profiles.all
  arrange(desc(Prediction)) # show the ice creams with the highest predicted rating on top

# use slice() to select rows
market_profiles <- profiles.all %>% 
  slice(c(4, 16, 23, 38)) # from profiles.all, select rows 4, 16, 23, 38 as the four profiles

market_profiles

conjoint_allrespondents <- conjoint(icecream, rvar = "rating", evar = c("Flavor","Packaging","Light","Organic"))

predict(conjoint_allrespondents, market_profiles) %>%
  arrange(desc(Prediction))

# same model as before, but now add by = "respondent"
conjoint_perrespondent <- conjoint(icecream, rvar = "rating", evar = c("Flavor","Packaging","Light","Organic"), by = "respondent")

predict(conjoint_perrespondent, market_profiles) %>% 
  arrange(respondent, desc(Prediction)) # sort by respondent and then by predicted rating

highest_rated <- predict(conjoint_perrespondent, market_profiles) %>% 
  group_by(respondent) %>% 
  mutate(ranking = rank(Prediction))

# have a look
highest_rated %>% 
  arrange(respondent, ranking)

# we need to retain only the highest ranked ice cream
highest_rated <- highest_rated %>% 
  arrange(respondent, ranking) %>% 
  filter(ranking == 4)

highest_rated

market_share <- highest_rated %>% 
  group_by(Flavor, Packaging, Light, Organic) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

market_share
