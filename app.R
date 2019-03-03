library(shiny)
library(tidyverse)
library(shinythemes)
library(scales)
library(ggrepel)
library(RColorBrewer)

options(scipen = 9999)
setwd("~/Rprace/attend collage/Attend")
csv_files <- list.files(pattern = ".csv")

df_deg <- read_csv("degrees-that-pay-back.csv",
                   col_names = c("major", "start_med_slry", "mid_car_slry",
                                 "percent_chng", "mid_car_10th", "mid_car_25th",
                                 "mid_car_75th", "mid_car_90th"),  # short names w/o whitespace
                   col_types = "cnndnnnn",  # specify column types to coerce '$' to numeric
                   skip = 1)  # names specified, skip header

df_col <- read_csv("salaries-by-college-type.csv",
                   col_names = c("school_name", "school_type", "start_med_slry",
                                 "mid_car_slry", "mid_car_10th", "mid_car_25th",
                                 "mid_car_75th", "mid_car_90th"),
                   col_types = "ccnnnnnn", skip = 1)

df_reg <- read_csv("salaries-by-region.csv",
                   col_names = c("school_name", "region", "start_med_slry",
                                 "mid_car_slry", "mid_car_10th", "mid_car_25th",
                                 "mid_car_75th", "mid_car_90th"),
                   col_types = "ccnnnnnn", skip = 1)

df_col <- df_col %>% filter(school_type != "Party")


ui <- tagList(
  navbarPage(
    theme = shinytheme("sandstone"), title="2nd shiny app",
    tabPanel("Distribution of salary",
             sidebarPanel(
               selectInput("region",
                           label = "Choose a region",
                           choices = unique(df_reg$region))
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Density",
                          plotOutput("wykres1")),
                 tabPanel("Histogram",
                          plotOutput("wykres2"))
               )
             )),
    tabPanel("Start slry vs Med slry",
             sidebarPanel(
               selectInput("szkoly",
                           label="Choose Universities",
                           choices = unique(df_reg$school_name),
                           multiple = TRUE,
                           selectize = TRUE),
               checkboxInput("smooth",
                             label="Add regression line",
                             value=FALSE),
               checkboxInput("se",
                             label="Display confidence interval",
                             value=FALSE),
               selectInput("kolor",
                           label="Choose a color of the span",
                           choices = c("blue","green","yellow","khaki",
                                       "purple"))
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Distribution",
                          plotOutput("wykres3"))
               )
             )),
    tabPanel("Best major",
             sidebarPanel(
               selectInput("kierunek",
                           label="Choose a major",
                           choices = unique(df_deg$major),
                           multiple = TRUE,
                           selectize = TRUE),
               checkboxInput("shadow",
                             label="Show future median salary",
                             value=FALSE)
               
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Degrees",
                          plotOutput("wykres4"))
               )
             )),
    tabPanel("School Type",
             sidebarPanel(
               selectInput("typ1",
                           label="Choose Engineering School",
                           choices = df_col$school_name[df_col$school_type == "Engineering"],
                           multiple = TRUE,
                           selectize = TRUE),
               selectInput("typ3",
                           label="Choose Arts School",
                           choices = df_col$school_name[df_col$school_type == "Liberal Arts"],
                           multiple = TRUE,
                           selectize = TRUE),
               selectInput("typ4",
                           label="Choose Ivy School",
                           choices = df_col$school_name[df_col$school_type == "Ivy League"],
                           multiple = TRUE,
                           selectize = TRUE),
               selectInput("typ5",
                           label="Choose State School",
                           choices = df_col$school_name[df_col$school_type == "State"],
                           multiple = TRUE,
                           selectize = TRUE)
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Types",
                          plotOutput("wykres5"))
               )
             ))
  )
)

server <- function(input, output) {
  
  output$wykres1 <- renderPlot({
    
    tabela1 <- df_reg %>% filter(region == input$region) %>%
      select(start_med_slry, mid_car_slry) %>%
      gather(timeline, salary) %>%
      mutate(timeline = as_factor(timeline, fct_rev))
    
    ggplot(data=tabela1, aes(x=salary)) +
      geom_density(alpha = 0.3, color = NA, aes(fill = timeline)) +
      scale_fill_manual(values = c('darkgreen', 'purple4')) +
      scale_x_continuous(labels = dollar, breaks=seq(30000,150000,15000)) + 
      theme(legend.position = "top",
          axis.text.y = element_blank(), axis.ticks.y = element_blank()) 
  })
  
  output$wykres2 <- renderPlot({
    
    tabela2 <- df_reg %>% filter(region == input$region) %>%
      select(start_med_slry, mid_car_slry) %>%
      gather(timeline, salary) %>%
      mutate(timeline = as_factor(timeline, fct_rev))
    
    ggplot(data=tabela2, aes(x=salary)) +
      geom_histogram(aes(y = ..density.., fill=timeline), alpha = 0.5, position = 'dodge') +
      scale_fill_manual(values = c('darkgreen', 'purple4')) +
      scale_x_continuous(labels = dollar, breaks=seq(30000,150000,15000)) + 
      theme(legend.position = "top",
            axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }) 
  
  output$wykres3 <- renderPlot({
    
    wektor_szkol <- input$szkoly
    
    ggplot(df_reg, aes(start_med_slry, mid_car_slry)) +
      geom_point(alpha=0.6, size=ifelse(df_reg$school_name %in% wektor_szkol, 3, 1.5),
                 color=ifelse(df_reg$school_name %in% wektor_szkol, "red", "black")) +
      geom_text_repel(data=subset(df_reg, school_name %in% wektor_szkol),aes(label=school_name), size = 3,
                      segment.size = 0.2, nudge_x = 10000, nudge_y = -15000) + 
      scale_x_continuous(labels = dollar, breaks=seq(30000,75000,5000)) +
      scale_y_continuous(labels = dollar, breaks=seq(40000,140000,10000)) +
      theme(axis.text.x= element_text(angle=45, hjust=1)) %>%
      {if (input$smooth) (geom_smooth(se = ifelse(input$se, T, F), 
                                      method="loess",color=input$kolor,span=2)) else NULL}
  })
  
  
  output$wykres4 <- renderPlot({
    
    wektor_kierunkow <- input$kierunek
    
    tabelka3 <- df_deg %>% filter(major %in% wektor_kierunkow)
    
    ggplot(data=tabelka3, aes(x = reorder(major, start_med_slry), y=start_med_slry)) +
      geom_col(fill = "darkgreen", alpha = 0.5) + 
      geom_text(aes(label = dollar(start_med_slry)), size = 3, hjust = 1.1) + 
      scale_y_continuous(labels = dollar) +
      xlab(NULL) + 
      coord_flip() +
      {if (input$shadow) (geom_col(aes(x = reorder(major, start_med_slry), y = mid_car_slry), alpha = 0.3)) else NULL}
    
  })
  
  output$wykres5 <- renderPlot({
    
    kolorki <- brewer.pal(n = 5, "Pastel1")
    
    wek1 <- input$typ1
    wek3 <- input$typ3
    wek4 <- input$typ4
    wek5 <- input$typ5
    
    wektor_duzy <- cbind(wek1, wek3, wek4, wek5)
    
    tabelka5 <- df_col %>%
      filter(school_name %in% wektor_duzy)
    
    ggplot(tabelka5, aes(x=reorder(school_name, mid_car_slry), y=mid_car_slry)) +
      geom_col(alpha = 0.8, aes(fill=school_type)) +
      scale_fill_manual(values = kolorki) +
      geom_text(aes(label = dollar(mid_car_slry)), hjust = 1.1, color = 'gray30') +
      scale_y_continuous(labels = dollar) +
      xlab(NULL) +
      coord_flip()
    
    
  })
}

shinyApp(ui = ui, server = server)

