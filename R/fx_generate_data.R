
# Function: Generate Data for Random Walk and Infect ----------------------

fx_generate_data <- function(.cases,
                             .steps,
                             .distance = 0.20,
                             .p_symptomatic = 0.40,
                             .p_death = 0.10,
                             .initial_infections = 1,
                             .incubation = 6,
                             .duration = 6) {

  library(dplyr)
  library(broom)
  library(tidyr)
  library(rlang)
  library(stringr)
  library(scales)
  library(tibble)
  library(trajr)
  library(purrr)

  # --- Event Listing ---
  # Stage 0: Healthy - Unexposed
  # Stage 1: Exposed - Incubation period, asymptomatic, able to spread
  # Stage 2: End of Incubation - Either Symptomatic or Asymptomatic, able to spread
  # Stage 3: End of Infection  - Either Recover or Death (A > R; S > R/D)

  patient_initial <- tibble(ID = seq(.cases))


  # Create map for status if ID gets exposed
  patient_stages <-
    patient_initial %>%
    mutate(seed_s2 = runif(n()),
           seed_s3 = runif(n()),
           stage_0 = "H",
           stage_1 = "E",
           ifexposed_stage_2 = ifelse(seed_s2 < .p_symptomatic, "S", "A"),
           ifexposed_stage_3 = case_when(ifexposed_stage_2 == "A" ~ "R",
                                         seed_s3 >= .p_death ~ "R",
                                         seed_s3 < .p_death ~ "D")) %>%
    select(ID, contains("stage"))


  # --- Figure out random walk for each ID
  patient_trajectory_nest <-
    patient_initial %>%
    mutate(x_seed = runif(.cases, min = -1, max = 1),
           y_seed = runif(.cases, min = -1, max = 1)) %>%
    rowwise() %>%
    mutate(random_walk =
             map(.steps, TrajGenerate, stepLength = 1, fps = 1) %>%
             map(select, x_traj = x, y_traj = y, time))

  patient_trajectory_long <-
    patient_trajectory_nest %>%
    unnest(random_walk) %>%
    group_by(ID) %>%
    arrange(time) %>%
    mutate(time = seq(0, n() - 1)) %>%
    ungroup() %>%
    mutate_at(vars(x_traj, y_traj), rescale_max, to = c(-1, 1)) %>%
    mutate(x = x_seed + x_traj,
           y = y_seed + y_traj) %>%
    select(time, ID, x, y)


  # --- Figure out when an ID is near another ID
  patient_time_nest <-
    patient_trajectory_long %>%
    group_by(time) %>%
    nest()

  patient_distance_nest <-
    patient_time_nest %>%
    transmute(distance = map(data, ~ .x %>%
                               column_to_rownames("ID") %>%
                               select(x, y) %>%
                               as.matrix() %>%
                               raster::pointDistance(lonlat = FALSE) %>%
                               broom::fix_data_frame() %>%
                               rowid_to_column("ID") %>%
                               pivot_longer(cols = X1:last_col(),
                                            names_to = "interaction_with",
                                            names_prefix = "X",
                                            names_ptypes = list(interaction_with = integer()),
                                            values_to = "distance") %>%
                               filter(distance != 0) %>%
                               mutate(flag_distance = distance <= .distance)))

  patient_possible_exposure <-
    patient_distance_nest %>%
    transmute(distance_flag = map(distance, filter, flag_distance)) %>%
    unnest(distance_flag) %>%
    ungroup()


  # --- Initalize R0
  R0 <- NULL


  # --- Track patient exposure
  patient_track <-
    patient_initial %>%
    sample_n(.initial_infections) %>%
    mutate(stage_0 = -1,
           stage_1 = 0,
           stage_2 = stage_1 + .incubation,
           stage_3 = stage_2 + .duration)

  for (i in 1:.steps) {

    .loop_infected <- patient_track %>% filter(stage_3 > i)

    # str_glue("Infected at Time {i}: {nrow(.loop_infected)}") %>% inform()

    .loop_data <-
      patient_possible_exposure %>%
      filter(time == i) %>%
      semi_join(.loop_infected, by = "ID") %>%
      mutate(stage_0 = -1,
             stage_1 = i,
             stage_2 = stage_1 + .incubation,
             stage_3 = stage_2 + .duration) %>%
      select(ID = interaction_with, contains("stage_"))

    patient_track <-
      patient_track %>%
      bind_rows(.loop_data) %>%
      arrange(stage_1) %>%
      distinct(ID, .keep_all = TRUE)

    # str_glue("-- Total Infected: {nrow(patient_track)}") %>% inform()

    # Track the R0 Rate of Infection
    .loop_R0 <- tibble(time = i, r0 = nrow(.loop_data) / nrow(.loop_infected))
    R0 <- bind_rows(R0, .loop_R0)

  }


  # --- Finalize R0
  R0 <-
    R0 %>%
    mutate(running_r0 = cummean(r0))


  # --- Bring it all together
  patient_track_long <-
    patient_track %>%
    pivot_longer(cols = -ID,
                 names_to = "stage",
                 values_to = "time")

  patient_stages_long <-
    patient_stages %>%
    pivot_longer(cols = -ID,
                 names_to = "stage",
                 names_prefix = "ifexposed_",
                 values_to = "status")

  patient_track_stages <-
    patient_track_long %>%
    left_join(patient_stages_long, by = c("ID", "stage"))

  # Label the Status by Stage
  patient_track_stages_status <-
    left_join(patient_trajectory_long,
              patient_track_stages,
              by = c("ID", "time")) %>%
    arrange(ID, time) %>%
    group_by(ID) %>%
    fill(stage, status) %>%
    ungroup() %>%
    replace_na(list(stage  = "stage_0",
                    status = "H"))

  # Function Output
  list(R0_df = R0,
       R0 = R0$r0 %>% mean(na.rm = TRUE),
       data = patient_track_stages_status)

}



# Test Function -----------------------------------------------------------

# source("R/fx_plot_spread.R")

# .steps <- 100
# .cases <- 50
# .distance <- 0.20
# .p_symptomatic <- 0.40
# .p_death <- 0.10
# .incubation <- 6
# .duration <- 6
# .initial_infections <-  5
#
# test <-
#   fx_generate_data(.cases = .cases,
#                    .steps = .steps,
#                    .distance = .distance,
#                    .p_symptomatic = .p_symptomatic,
#                    .p_death = .p_death,
#                    .incubation = .incubation,
#                    .duration = .duration,
#                    .initial_infections = .initial_infections)

# test$data %>% fx_plot_infection_spread()
