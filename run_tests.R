
library(here)
library(testthat)
library(devtools)
# this is done locally for testing purposes. Change for reproducibility
install("~/Desktop/greeting_R_package")

library(GreetingRPackage)


test_dir_path <- here("Desktop", 'greeting_R_package',"tests", "testthat")

test_results <- test_dir(test_dir_path)

print(test_results)