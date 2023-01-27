library(shiny)
library(tidyverse)
library(nflplotR)
library(nflreadr)
library(ggrepel)




pbp = load_pbp(2022)

qbs = pbp |> 
  filter(pass == 1) |> 
  group_by(passer) |> 
  summarise(n = n()) |> 
  filter(n > 10) |> 
  pull(passer) |>
  unique()

# Define UI for application that draws a histogram
ui = navbarPage(
        title = 'Impact of QB Mistakes',
        tabPanel(
          title = 'Graph',
          titlePanel(title = 'EPA Lost on QB Mistakes'),
          sidebarLayout(
            sidebarPanel(
              checkboxInput(inputId = 'int',
                            label = 'Interception (mistake):',
                            value = 1),
              checkboxInput(inputId = 'fum',
                            label = 'Fumble (mistake):',
                            value = 1),
              checkboxInput(inputId = 'sack',
                            label = 'Sack (mistake):',
                            value = 1),
              # selectInput(inputId = 'numb',
              #             label = 'Min Number of Mistakes:',
              #             choices = 1:50,
                          # selected = 1),
              sliderInput(inputId = 'winp',
                          label = 'Win Percentage:',
                          min = 0,
                          max = 100,
                          value = c(20, 80),
                          step = 1),
              sliderInput(inputId = 'expts',
                          label = 'Expected Points:',
                          min = -100,
                          max = 100,
                          value = c(-100, 100),
                          step = 100)
              
            ),
            mainPanel(plotOutput('plot'))
          )
        ),
        tabPanel(title = 'Table', dataTableOutput('table')),
        tabPanel(title = 'About', includeMarkdown('about.Rmd'))
)

# Define server logic required to draw a histogram
server = function(input, output) {

    output$plot = renderPlot({
      pbp |> 
        filter(case_when(input$sack ~ sack == 1, !input$sack ~ NA) | 
               case_when(input$int ~ interception == 1, !input$int ~ NA) | 
               (case_when(input$fum ~ fumble == 1, !input$fum ~ NA) 
                      & (fumbled_1_player_name == passer | fumbled_2_player_name == passer |
                         fumbled_1_player_name == rusher | fumbled_2_player_name == rusher)), 
              (pass == 1 & passer %in% qbs) | (rush == 1 & (rusher %in% qbs)), 
               wp*100 >= input$winp[1], wp*100 <= input$winp[2], 
               ep >= input$expts[1], ep <= input$expts[2]) |>
        mutate(player = case_when(!is.na(passer) ~ passer, !is.na(rusher) ~ rusher)) |> 
        # mutate(player_id = case_when(!is.na(passer) ~ passer_id, !is.na(rusher) ~ rusher_id)) |> 
        group_by(player, posteam) |> 
        summarise(epa_lost_rate = mean(qb_epa), epa_lost = sum(qb_epa), n = n(), .groups = 'drop') |> 
        filter(n >= 1) |> 
        
        ggplot(aes(x=n, y = epa_lost_rate)) +
        geom_point(aes(color = posteam, fill = posteam)) +
        labs(x = 'Number of Mistakes',
             y = 'EPA Lost per Mistake') +
        stat_smooth(formula = y ~ x, method = 'lm', geom = 'line', se=FALSE, color='gray') +
        scale_y_reverse() +
        theme_bw() +
        nflplotR::geom_mean_lines(aes(h_var=n, v_var = epa_lost_rate)) + 
        geom_text_repel(aes(label=player), size =2.5) + 
        nflplotR::scale_color_nfl(type = 'primary') + 
        nflplotR::scale_fill_nfl(type = 'secondary', alpha = 0.5)
    })
    
    output$table = renderDataTable({
      pbp |> 
        filter(case_when(input$sack ~ sack == 1, !input$sack ~ NA) | 
                 case_when(input$int ~ interception == 1, !input$int ~ NA) | 
                 (case_when(input$fum ~ fumble == 1, !input$fum ~ NA) 
                  & (fumbled_1_player_name == passer | fumbled_2_player_name == passer |
                       fumbled_1_player_name == rusher | fumbled_2_player_name == rusher)), 
               (pass == 1 & passer %in% qbs) | (rush == 1 & (rusher %in% qbs)), 
               wp*100 >= input$winp[1], wp*100 <= input$winp[2], 
               ep >= input$expts[1], ep <= input$expts[2]) |>
        mutate(player = case_when(!is.na(passer) ~ passer, !is.na(rusher) ~ rusher)) |> 
        # mutate(player_id = case_when(!is.na(passer) ~ passer_id, !is.na(rusher) ~ rusher_id)) |> 
        group_by(player, posteam) |> 
        summarise(epa_lost_rate = mean(qb_epa), epa_lost = sum(qb_epa), n = n(), .groups = 'drop') |> 
        filter(n >= 1)
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
