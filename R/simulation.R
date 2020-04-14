
# Package 'trajr' ---------------------------------------------------------

pacman::p_load(tidyverse, trajr, plotly, scales, rlang, gganimate, gifski)
# raster::pointDistance()

cases <- 60
steps <- 60
.distance <- 0.2

patient_init <-
  tibble(id = seq(cases),
         x_0 = runif(cases, min = -1, max = 1),
         y_0 = runif(cases, min = -1, max = 1)) %>%
  print()

patient_steps <-
  patient_init %>%
  rowwise() %>%
  mutate(steps =
           map(steps, TrajGenerate, stepLength = 1, fps = 1) %>%
           map(select, x_1 = x, y_1 = y, time)) %>%
  print()

patient_steps_long <-
  patient_steps %>%
  unnest(steps) %>%
  group_by(id) %>%
  arrange(time) %>%
  mutate(step = seq(0, n() - 1)) %>%
  ungroup() %>%
  mutate_at(vars(x_1, y_1), rescale_max, to = c(-1, 1)) %>%
  mutate(x = x_0 + x_1,
         y = y_0 + y_1) %>%
  select(id, step, x, y) %>%
  print()

patient_zero <-
  patient_steps_long %>%
  filter(step == 0) %>%
  sample_n(1) %>%
  mutate(status = "infected") %>%
  print()

.infected_patients <- patient_zero %>% select(step, id, status)

for (i in seq(steps)) {
# for (i in 1:12) {

  str_glue("Step: {i} - Cases: {n_distinct(.infected_patients$id)}") %>% message()

  .infected_ids_for_step_i <-
    .infected_patients %>%
    distinct(id) %>%
    pull()

  .table_for_step_i_input <-
    patient_steps_long %>%
    filter(step == i)

  .table_spread <-
    .table_for_step_i_input %>%
    column_to_rownames("id") %>%
    select(x, y) %>%
    as.matrix() %>%
    raster::pointDistance(lonlat = FALSE) %>%
    broom::fix_data_frame() %>%
    rowid_to_column("id") %>%
    pivot_longer(cols = X1:last_col(),
                 names_to = "interacted_with",
                 names_prefix = "X",
                 values_to = "distance") %>%
    filter(distance != 0) %>%
    mutate(infected_person  = id  %in% .infected_ids_for_step_i,
           infected_contact = interacted_with %in% .infected_ids_for_step_i,
           close_proximity  = distance <= .distance,
           spread = (infected_person | infected_contact) & close_proximity)

  # Print Results for Debugging
  # .table_spread %>%
  #   filter(id %in% c(patient_zero$id)) %>%
  #   arrange(distance) %>%
  #   print()

  .table_output <-
    .table_spread %>%
    filter(spread | infected_person) %>%
    mutate(step = i, status = "infected") %>%
    distinct(step, id, status)

  .infected_patients <-
    bind_rows(.infected_patients,
              .table_output)

} # End For Loop

patient_infected_per_step <-
  patient_steps_long %>%
  left_join(.infected_patients, by = c("step", "id")) %>%
  replace_na(replace = list(status = "healthy")) %>%
  mutate(color = ifelse(status == "infected", "#de2d26", "#9ecae1"))

plot_axes <-
  list(zeroline = FALSE,
       linecolor = "black",
       mirror = "ticks",
       showticklabels = FALSE,
       title = "")

patient_infected_per_step %>%
  plot_ly(x = ~x,
          y = ~y,
          text = ~str_glue("ID: {id}\n- Step: {step}\n- Status: {status}"),
          hoverinfo = "text",
          frame = ~step,
          type = "scatter",
          mode = "markers",
          marker = list(color = ~color, size = 10)) %>%
  layout(xaxis = plot_axes, yaxis = plot_axes, showlegend = FALSE,
         title = list(text = "Simulating the Spread of Infection",
                      font = list(family = "Avenir", size = 18),
                      xref = "container",
                      x = 0))


# Manual Usage ------------------------------------------------------------

