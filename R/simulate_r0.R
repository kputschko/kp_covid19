
# Finding R0 Values -------------------------------------------------------

source("R/fx_generate_data.R")

parameter_map <-
  list(.steps = 60,
       .cases = 100,
       .distance = seq(0.10, 0.30, by = 0.05),
       .p_symptomatic = seq(0.30, 0.60, by = 0.05),
       # .p_death = seq(0.05, 0.2, by = 0.05),
       .p_death = 0.10,
       .initial_infections = 1:3,
       .incubation = 5:10,
       .duration = 5:10) %>%
  cross_df()


# 30 Minutes for 1000
df_test <-
  parameter_map %>%
  sample_n(1000)

system.time({

  r0_test <-
    df_test %>%
    pmap(fx_generate_data) %>%
    map_dbl("R0")

})[[3]]

df_output <- df_test %>% add_column(R0 = r0_test)

df_output %>% write_csv("data/r0_sim_output.csv")



# Get R0 Estimates --------------------------------------------------------

lookup_table <-
  df_output %>%
  arrange(R0) %>%
  filter(R0 > 0) %>%
  mutate(rob = round(R0, digits = 1))

lookup_table %>% write_csv("data/r0_lookup_table.csv")

# lookup_table %>% count(rob) %>% view()
# selected_r0 <- 3.2
# lookup_table %>%
#   filter(rob == selected_r0) %>%
#   sample_n(1)
