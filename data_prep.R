library(dplyr)
library(tidyr)
library(tibble)

# attach data frame from 'mtcars' dataset to global environment
mtcars <- datasets::mtcars

# convert the rownames to an actual column.  We suggest to never use row names, just
# use a regular column.
mtcars <- rownames_to_column(mtcars, var = 'model')

# Converting Weight (i.e. 'wt') from 1000's of lbs to lbs
mtcars$wt <- mtcars$wt * 1000


# Converting binary values to intended, character values
mtcars <- mtcars %>%
  mutate(
    vs = ifelse(vs == 0, 'V-shaped', 'Straight'),
    am = ifelse(am == 0, 'Automatic', 'Manual')
  )


saveRDS(mtcars, file = '01_traditional/data_prep/prepped/mtcars.RDS')


saveRDS(read.csv("data/CMR_Registry_CRF_KHChoi.csv", check.names = F), "data/eCRFexam.RDS")

saveRDS(tibble(pid = paste0("R-", 1:200), Group = randomizr::complete_ra(N = 200, conditions = c("comparator", "target"))), "data/random.RDS")
 