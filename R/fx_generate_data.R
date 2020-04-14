
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


  # Create 'Score Board' tracking when each ID enters each stage
  # The values for each stage_ are the time at which that ID enters that stage
  # This could be pivoted to long format: ID, Time, X, Y, Stage

  # patient_inital <-
  #   tibble(ID = seq(.cases),
  #          stage_0 = 0,
  #          stage_1 = ifelse(ID == sample(ID, 1), 0, NA),
  #          stage_2 = na_dbl,
  #          stage_3 = na_dbl) %>%
  #   print()

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
    select(ID, contains("stage")) %>%
    print()


  # --- Figure out random walk for each ID
  patient_trajectory_nest <-
    patient_initial %>%
    mutate(x_seed = runif(.cases, min = -1, max = 1),
           y_seed = runif(.cases, min = -1, max = 1)) %>%
    rowwise() %>%
    mutate(random_walk =
             map(.steps, TrajGenerate, stepLength = 1, fps = 1) %>%
             map(select, x_traj = x, y_traj = y, time)) %>%
    print()

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
    select(time, ID, x, y) %>%
    print()


  # --- Figure out when an ID is near another ID
  patient_time_nest <-
    patient_trajectory_long %>%
    group_by(time) %>%
    nest() %>%
    print()

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
                               mutate(flag_distance = distance <= .distance))) %>% print()

  patient_possible_exposure <-
    patient_distance_nest %>%
    transmute(distance_flag = map(distance, filter, flag_distance)) %>%
    unnest(distance_flag) %>%
    ungroup() %>%
    print()


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

    str_glue("Infected at Time {i}: {nrow(.loop_infected)}") %>%
      inform()

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

    str_glue("-- Total Infected: {nrow(patient_track)}") %>%
      inform()

  }


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
  patient_track_stages_status




# Testing - Old Stuff - Previous Ways of Thinking -------------------------

  # Get the PLOT RIGHT
  # testingplot <-
  #   patient_track_stages_status %>%
  #   filter(ID %in% 1:5,
  #          time <= 10)
  #
  # testingplot %>% filter(ID == 5)
  #
  # testingplot %>%
  #   plot_ly(x = ~x,
  #           y = ~y,
  #           frame = ~time,
  #           text = ~str_glue("ID: {ID}\n- Time: {time}\n- Status: {status}"),
  #           hoverinfo = "text",
  #           type = "scatter",
  #           marker = list(color = ~color, size = 11),
  #           mode = "markers")
  #
  #
  #   fx_plot_infection_spread()


  # Old Shit ----------------------------------------------------------------

  # .loop_seq <- seq(1, .steps)
  # .loop_collection <- list_along(.loop_seq) %>% set_names(nm = str_c("time_", .loop_seq))
  # .loop_collection$`time_0` <- patient_zero
  #
  # for (i in .loop_seq) {
  #
  #   .i_label_current  <- str_c("time_", i)
  #   .i_label_previous <- str_c("time_", i - 1)
  #
  #   .i_table <- patient_possible_spread %>% filter(time == i)
  #
  #   .i_infected_previous <-
  #     .loop_collection[[.i_label_previous]] %>%
  #     filter(i < time_2)
  #
  #   .i_infected_current <-
  #     .i_table %>%
  #     ungroup() %>%
  #     select(time:interaction_with) %>%
  #     semi_join(.i_infected_previous, by = c("interaction_with" = "ID")) %>%
  #     left_join(patient_initalize %>% select(ID, status_0:status_2), by = "ID") %>%
  #     mutate(flag_0 = TRUE,
  #            flag_1 = flag_0,
  #            flag_2 = status_1 == "symptomatic",
  #            time_0 = i,
  #            time_1 = time_0 + .incubation,
  #            time_2 = time_1 + .duration) %>%
  #     select(ID, flag_0, time_0, status_0, flag_1, time_1, status_1, flag_2, time_2, status_2)
  #
  #   .i_infected_cumulative <-
  #     .i_infected_previous %>%
  #     bind_rows(.i_infected_current)
  #
  #   .loop_collection[[.i_label_current]] <- .i_infected_cumulative
  # }
  #
  # .loop_collection %>%
  #   enframe() %>%
  #   mutate(time = str_remove(name, "time_") %>% as.integer()) %>%
  #   select(time, value) %>%
  #   unnest(value) %>%
  #   arrange(time)
  #
  # # New Idea: Four lists: healthy, exposed, symp/asymp, dead/rec
  # # tibble(time = seq(0, .steps),
  # #        event_0 = na_int,
  # #        event_1 = na_int,
  # #        event_2 = na_int) %>%
  # #   mutate(event_init = list(seq(.cases)),
  # #          event_0 = case_when(time == 0 ~ map_dbl(event_init, sample, 1)),
  # #          event_init = map(event_init, setdiff, event_0))
  #
  # # THE GOAL
  # # ID, X, Y, TIME, STATUS
  #
  #
  #
  #
  #
  #
  # # --- Prevous Attempt
  # # patient_zero <-
  # #   patient_steps_long %>%
  # #   filter(step == 0) %>%
  # #   sample_n(1) %>%
  # #   mutate(status = ifelse(id_seed < .p_asymptomatic, "asymptomatic", "symptomatic"))
  #
  # .infected_patients <- patient_zero %>% select(step, id, id_seed, status)
  #
  # # for (i in seq(steps)) {
  # for (i in 1:9) {
  #
  #   str_glue("Step: {i} - Cases: {n_distinct(.infected_patients$id)}") %>% message()
  #
  #   .change_in_status <-
  #     .symptomatic_patients %>%
  #     group_by(id, status) %>%
  #     mutate(sequence = sequence(n()),
  #            elevation = case_when(sequence > .duration & id_seed <= .p_death ~ "dead",
  #                                  sequence > .duration & id_seed > .p_death ~ "recovered"),
  #            status_2 = coalesce(elevation, status)) %>%
  #     ungroup()
  #
  #   .id_status_continue <-
  #     .change_in_status %>%
  #     filter(step == max(step), status_2 %in% c("asymptomatic", "symptomatic")) %>%
  #     distinct(id) %>%
  #     pull(id)
  #
  #   .id_status_dropout <-
  #     .change_in_status %>%
  #     filter(step == max(step), status_2 %in% c("dead", "recovered")) %>%
  #     distinct(id) %>%
  #     pull(id)
  #
  #   # .symptomatic_ids_for_step_i <-
  #   #   .change_in_status %>%
  #   #   filter(step == max(step),
  #   #          status_2 %in% c("asymptomatic", "symptomatic")) %>%
  #   #   distinct(id) %>%
  #   #   pull()
  #
  #
  #   .table_for_step_i_input <-
  #     patient_steps_long %>%
  #     filter(step == i)
  #
  #   .table_spread <-
  #     .table_for_step_i_input %>%
  #     column_to_rownames("id") %>%
  #     select(x, y) %>%
  #     as.matrix() %>%
  #     raster::pointDistance(lonlat = FALSE) %>%
  #     broom::fix_data_frame() %>%
  #     rowid_to_column("id") %>%
  #     pivot_longer(cols = X1:last_col(),
  #                  names_to = "interacted_with",
  #                  names_prefix = "X",
  #                  values_to = "distance") %>%
  #     filter(distance != 0) %>%
  #     mutate(symptomatic_person  = id  %in% .symptomatic_ids_for_step_i,
  #            symptomatic_contact = interacted_with %in% .symptomatic_ids_for_step_i,
  #            close_proximity  = distance <= .distance,
  #            spread = (symptomatic_person | symptomatic_contact) & close_proximity)
  #
  #   .table_output <-
  #     .table_spread %>%
  #     left_join(.table_for_step_i_input %>% select(id, id_seed), by = "id") %>%
  #     filter(spread | symptomatic_person) %>%
  #     mutate(step = i,
  #            status = ifelse(id_seed < .p_asymptomatic, "asymptomatic", "symptomatic")) %>%
  #     distinct(step, id, status, id_seed)
  #
  #   .symptomatic_patients <-
  #     bind_rows(.symptomatic_patients,
  #               .table_output)
  #
  # } # End For Loop
  #
  # color_map <-
  #   c(symptomatic  = "#de2d26",
  #     healthy   = "#9ecae1",
  #     asymptomatic   = "#F3D2AF",
  #     dead      = "gray",
  #     recovered = "#9ecae1")
  #
  # # .symptomatic_patients %>%
  # #   group_by(id, status) %>%
  # #   mutate(sequence = sequence(n()),
  # #          elevation = case_when(sequence > .duration & id_seed <= .p_death ~ "dead",
  # #                                sequence > .duration & id_seed > .p_death ~ "recovered"),
  # #          status_2 = coalesce(elevation, status)) %>%
  # #   view()
  #
  # patient_symptomatic_per_step <-
  #   patient_steps_long %>%
  #   left_join(.symptomatic_patients, by = c("step", "id", "id_seed")) %>%
  #   replace_na(replace = list(status = "healthy")) %>%
  #   group_by(id, status) %>%
  #   mutate(sequence = sequence(n()),
  #          color = recode(status, !!!color_map))
  #
  # # Loop Output
  # patient_symptomatic_per_step

}



# Test Function -----------------------------------------------------------

source("R/fx_plot_spread.R")

.steps <- 100
.cases <- 50
.distance <- 0.20
.p_symptomatic <- 0.40
.p_death <- 0.10
.incubation <- 6
.duration <- 6
.initial_infections <-  5

test <-
  fx_generate_data(.cases = .cases,
                   .steps = .steps,
                   .distance = .distance,
                   .p_symptomatic = .p_symptomatic,
                   .p_death = .p_death,
                   .incubation = .incubation,
                   .duration = .duration,
                   .initial_infections = .initial_infections)

# test %>% fx_plot_infection_spread()
