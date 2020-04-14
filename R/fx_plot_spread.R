
# Function: Plot and Animate Infection Spread -----------------------------

fx_plot_infection_spread <- function(data,
                                     title = "Simulating the Spread of Infection",
                                     .color_map = c(H = "#9ecae1",
                                                    E = "#F3D2AF",
                                                    A = "#F3D2AF",
                                                    S = "#de2d26",
                                                    R = "#9ecae1",
                                                    D = "gray")) {

  library(plotly)

  .data <- data %>% mutate(color = recode(status, !!!color_map))


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
            marker = list(color = ~color, size = 11)) %>%
    layout(xaxis = plot_axes, yaxis = plot_axes, showlegend = FALSE,
           title = list(text = "Simulating the Spread of Infection",
                        font = list(family = "Avenir", size = 18),
                        xref = "container",
                        x = 0))

}
