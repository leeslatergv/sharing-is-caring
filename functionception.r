# Function
# Function-ception! Step by step
# Step 1 - open a new function - pass the arguments 'data' and 'question'.
merged_demographic_table_function <- function(data = responses_clean, question) {
  # Step 2 - going to need a function within this to create a demographic table. 
  # Step 2a - Pass data, demographic, and the question as before.
    demographic_function <- function(data = responses_clean, demographic, question){
      #  Step 2b - use your multivariate single code function made previously to make a demographic table
      dataobject1 <- data %>% multivariate_singlecode_ft(question_1 = demographic, question_2 = question)
      dataobject1 <- dataobject1 %>% pivot_wider(names_from = ".[[question_1]]", values_from = c('Frequency', 'Percentage', 'Total base')) %>%
       # Step 2c - I changed the way naming worked. This approach will take the question you when specifying argument 'question' - more dynamic.
         dplyr::rename(!!question := `.[[question_2]]`)
    }
  # Step 3 - now the demographic function is made, we just run this through our 6 demographic categories.
  dem_respondent_type <- demographic_function(data = responses_clean, demographic = "are_you_completing_this_consultation_as", question = question)
  dem_ethnicity <- demographic_function(data = responses_clean, demographic = "what_is_your_ethnic_group", question = question)
  dem_age <- demographic_function(data = responses_clean, demographic = "how_old_are_you", question = question)
  dem_sex <- demographic_function(data = responses_clean, demographic = "what_is_your_sex", question = question)
  dem_religion <- demographic_function(data = responses_clean, demographic = "what_is_your_religion", question = question)
  dem_uk <- demographic_function(data = responses_clean, demographic = "which_area_of_the_uk_do_you_live_in", question = question)
  # Step 4 - now we combine the 6 tables using your elegant reduce solution.
  merged_table <- list(dem_respondent_type, dem_ethnicity, dem_age, dem_sex, dem_religion, dem_uk) %>%
        purrr::reduce(left_join, by = question)
}
# To use the function, simply pass the question you want to 'question', and save as output.
Q8_covid_healthcare_demographics <- merged_demographic_table_function(data = responses_clean, question = "Q8_must_have_covid_vaccine_healthcare")
