
# Function: Plot and Animate Infection Spread -----------------------------

fx_plot_infection_spread <- function(data,
                                     title = "Simulating the Spread of Infection",
                                     .color_map = c(H = "#0C7BDC",
                                                    E = "#FFC20A",
                                                    A = "#FFC20A",
                                                    S = "#DC3220",
                                                    R = "#6081E3",
                                                    D = "translucent")) {

  library(plotly)

  .line_map <-
    c(H = "translucent",
      E = "translucent",
      A = "#translucent",
      S = "#translucent",
      R = "translucent",
      D = "darkgray")

  .data <-
    data %>%
    arrange(time, ID) %>%
    mutate(color = recode(status, !!!.color_map),
           line  = recode(status, !!!.line_map))


  plot_axes <-
    list(zeroline = FALSE,
         linecolor = "black",
         mirror = "ticks",
         showticklabels = FALSE,
         title = "")

  .data %>%
    plot_ly(x = ~x,
            y = ~y,
            frame = ~time,
            text = ~str_glue("ID: {ID}\n- Step: {time}\n- Status: {status}"),
            hoverinfo = "text",
            type = "scatter",
            mode = "markers",
            marker = list(color = ~color, size = 12,
                          line = list(color = ~line,
                                      width = 1))) %>%
    layout(xaxis = plot_axes, yaxis = plot_axes, showlegend = FALSE,
           plot_bgcolor = "black", paper_bgcolor = "translucent",
           title = list(text = "Simulating the Spread of Infection",
                        font = list(family = "Avenir", size = 18),
                        xref = "container",
                        x = 0)) %>%
    animation_slider(currentvalue = list(prefix = "Time: "))

}


# Test Function -----------------------------------------------------------

# source("R/fx_generate_data.R")
# test_data <- fx_generate_data(.cases = 50, .steps = 50)
# test_data %>% fx_plot_infection_spread()
