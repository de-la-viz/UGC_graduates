#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# TO DO:
#   use functions for the plots (2 functions)

#########################
# Data load and cleanup #
#########################
# We do this outside the server, so that it's run only once!

# Import libraries
library(shiny) # shiny app
library(ggplot2) # plotting
library(dplyr) # filtering and manipulating data
library(haven) # to use as_factor()
library(rio) # good package for importing data
library(stringr) # to use str_sub and extract year from academic year
library(forcats) # for fct_rev to reverse factor levels
library(scales) # for scales as percentages
library(shinythemes) # change the theme of the app

# Import data
graduates <- rio::import("data/Graduates2(Eng).csv") %>%
  mutate(Level = as_factor(`Level of Study`),
         Year = as.integer(str_sub(`Academic Year`, start = 1L, end = 4)), # first year of Academic year for the slider.
         Academic_Year = as_factor(`Academic Year`),
         Category = as_factor(`Broad Academic Programme Category` ),
         Gender = as_factor(Sex),
         Headcount = `Number of Graduates (Headcount)`
  ) %>% # rename and convert to factor
  select(Year, Gender, Level, Category, Headcount) %>%
  mutate(Category = fct_relevel(Category, # manual relevel for nicer plots
                                "Arts and Humanities", "Education", "Social Sciences", 
                                "Medicine, Dentistry and Health", "Business and Management", 
                                "Sciences", "Engineering and Technology"))

category_names = levels(graduates$Category) # get the names of all categories
level_names = levels(graduates$Level)  # get the names of all levels

# Define color palette:
# order: black, male, female, other, highlight
my_palette <- c("#383D3B", "#52DEE5", "#B3B3F1", "#FF220C", "#BEEF9E")


##################
# User Interface #
##################

# generic line initiating the UI
ui <- shinyUI(fluidPage(
  # change theme:
  theme = shinytheme("slate"),
  
  # Add a title
  titlePanel("Gender Balance among Graduates of UGC-funded Programmes"),
  
  # This creates a layout with a left sidebar and main section
  sidebarLayout(
    position = "right", # sidebar to the right.
    
    # beginning of sidebar section
    sidebarPanel(
      em("Francois Delavy, May 2020 | ", a(href = "https://github.com/de-la-viz/UGC_graduates", "code on Github")),
      width = 12
    ),
    
    # beginning of main section
    mainPanel(
      p("Annually, 28'500 students graduate from programs funded by the ",
        a(href = "https://www.ugc.edu.hk/eng/ugc/index.html", "University Grants Committee"),
        "(UGC). The mission of the UGC is to nurture high-quality people to promote the economic and 
        social development of Hong Kong. Are ",
        span(strong("male"), style = "color:#52DEE5"), 
        " and ",
        span(strong("female"), style = "color:#B3B3F1"), 
        "students equally represented among the graduates who will build the future of Hong-Kong?"),
      
      # several tabs:
      tabsetPanel(
        tabPanel("Gender Balance",
                 
                 br(), # a blank line 
                 
                 plotOutput("gender_balance"),
                 
                 br(), # a blank line 
                 
                 fluidRow(
                   column(6,
                          radioButtons("y_axis", p("Explore the gender balance by selecting another variable of interest:"),
                                       choices = list("Broad Academic Programme Category (APC)" = "Category", 
                                                      "Level of Study" = "Level"), selected = "Category")
                   ),
                   column(6, # width on a max of 12 columns
                          sliderInput("year_slider", # name for the widget, to call it
                                      p("Or by changing the year range over which the percentages are computed:"), # label displayed in app
                                      min = min(graduates$Year), max = max(graduates$Year), 
                                      value = c(min(graduates$Year), max(graduates$Year)), # initial selection
                                      ticks = FALSE,
                                      step = 1, # steps of 1 year
                                      sep = '' # no separators btw thousands places in numbers
                          )
                   )
                 )
        ),
        
        tabPanel("Time Trends: APC",
                 br(),
                 # the buttons
                 fluidRow(
                   column(6, # width on a max of 12 columns
                          selectInput("filter_line_cat", label = p("Choose an Academic Programme Category"),
                                      choices = list("All Categories" = "All Categories",
                                                     "Arts and Humanities" = "Arts and Humanities",
                                                     "Business and Management" = "Business and Management",
                                                     "Education" = "Education",
                                                     "Engineering and Technology" = "Engineering and Technology",
                                                     "Medicine, Dentistry and Health" = "Medicine, Dentistry and Health",
                                                     "Sciences" = "Sciences",
                                                     "Social Sciences" = "Social Sciences"),
                                      selected = "All Categories")
                   ),
                   
                   column(6,
                          radioButtons("num_or_perc_cat", label = p(""),
                                       choices = list("Headcount" = "Headcount",
                                                      "Percentage" = "Percentage"),
                                       selected = "Headcount"
                                       
                          )
                   )
                 ),
                 
                 plotOutput("time_trends_APC")
        ),
        
        tabPanel("Time Trends: Level",
                 br(),
                 # the buttons
                 fluidRow(
                   column(6, # width on a max of 12 columns
                          selectInput("filter_line_level", label = p("Choose a Study Level"),
                                      choices = list("All Levels" = "All Levels", 
                                                     "Sub-degree" = "Sub-degree",
                                                     "Undergraduate" = "Undergraduate",
                                                     "Taught Postgraduate" = "Taught Postgraduate",
                                                     "Research Postgraduate" = "Research Postgraduate"),
                                      selected = "All Levels")
                   ),
                   
                   column(6,
                          radioButtons("num_or_perc_level", label = p(""),
                                       choices = list("Headcount" = "Headcount",
                                                      "Percentage" = "Percentage"),
                                       selected = "Headcount"
                                       
                          )
                   )
                 ),
                 
                 plotOutput("time_trends_Level")
                 
        ),
        
        tabPanel("Key Observations",
                 
                 br(),
                 p("We highlight 3 key observations that we made when exploring the data.
                   These actionable insights aim at informing decision-makers: UGS programme leads or other policymakers. 
                   This project also highlights the importance of open data for democratic 
                   control of government-funded institutions and for informing the public and politicians."),
                 br(),
                 br(),
                 br(),
                 
                 p(span(strong("The gender balance varies greatly by Academic Programme Category.")), 
                   "Some categories are dominated by female graduates and others by male.
                   On average, over the 10 academic years considered, 54.9% of the graduates were women."),
                 # p("-> UGC and the higher education institutions could work on uniformizing the gender balance."),
                 plotOutput("imbalance_by_APC"),
                 br(),
                 br(),
                 br(),
                 
                 p(span(strong("The gender imbalance in Engineering and Technology is increasing.")), 
                   "Despite the overall growth of the number of graduates, 
                   the number of female graduates has remained stable.
                   The percentage of men went up from to 63.8% in 2009/10 to 69.5% in 2018/19."),
                 # p("-> UGC and the higher education institutions could act to try reversing the trend."),
                 plotOutput("engineering"),
                 br(),
                 br(),
                 br(),
                 
                 p(span(strong("A majority of the graduates are female for Sub-degree, 
                   Undergraduate and Taught Postgraduate levels, 
                   whereas the majority of graduates are males for the Research Postgraduate level.")), 
                   "This finding is similar to what is observed in several other countries 
                   and might be an indication that there is a 'leaky pipeline' effect: the fact that women 
                   tend to be less and less represented within researcher population with age (or experience, career level). 
                   See for instance the ",
                   a(href = "https://euraxess.ec.europa.eu/worldwide/japan/status-update-gender-equality-research-careers-europe-she-figures-2018",
                     "European She Figures 2018"), ".
                   "),
                 # p("-> UGC could work on attracting more males to lower levels of study and 
                 #     retaining more females at the highest level of study."),
                 plotOutput("leaky_pipeline"),
                 br(),
                 br(),
                 br()
        ),
        
        tabPanel("About the Data",
                 br(),
                 p("The dataset can be downloaded from ",
                   a(href = "https://data.gov.hk/en-data/dataset/hk-ugc-ugc-student-graduates2", "data.gov.hk"),                 
                   ". Alternatively, the same data are also found on the",
                   a(href = "https://cdcf.ugc.edu.hk/cdcf/searchStatSiteReport.action", "UGC website"),
                   "in PDF format, and ",
                   a(href = "www.ugcs.gov.hk/datagovhk/Graduates2_data_dictionary_en.docx", "here"),
                   "is a data dictionary."
                 ),   
                 
                 p("UGC shares the number of graduates (Headcount) along with several variables:"),
                 tags$ul(
                   tags$li("Sex (gender): Male, Female"), 
                   tags$li("Level of Study: Sub-degree, Undergraduate, Taught Postgraduate, Research Postgraduate"), 
                   tags$li("Broad Academic Programme Category: Medicine, Dentistry and Health; Sciences; Engineering and Technology; Business and Management; Social Sciences; Arts and Humanities, Education"),
                   tags$li("Academic Year: YYYY/YY, from 2009/10 to 2018/19. We show only the first year of the academic year in the plots.")
                 ),
                 
                 p("Since some UGC-funded programmes are mapped to more than one Academic Programme Category (APC), graduate numbers of these programmes are counted across the APCs concerned on a pro-rata basis. The decimal figures are rounded to the nearest whole number."),   
                 
                 p("‘Headcount’ refers to a simple count of numbers of graduates without taking into account the study mode (i.e. full-time or part-time study).")
                 
        )
        
        
      )
    )
  )
  
  # Close the UI definition
))


##########
# SERVER #
##########

# generic line initiating the SERVER 
server <- shinyServer(function(input, output) {
  
  #############
  # Reactives #
  #############
  # define any reactive elements of the app

  output$test <- renderText({
    paste("Here is the output: ", input$y_axis) # note: y_axis is the name of the radioButtons defined in UI
  })
  
  # ----- Everything needed for the first tab, gender_balance  
  # data for the gender_balance plot
  gender_balance_data <- reactive({
    if ("Category" %in% input$y_axis) {     
      gender_balance_data <- graduates %>% 
        filter(Year >= input$year_slider[1], 
               Year <= input$year_slider[2]) %>% # filter for the years chosen by slider
        group_by(Category, Gender) %>%
        summarise(Headcount = sum(Headcount)) %>%
        group_by(Category) %>%
        mutate(percentage = Headcount/sum(Headcount),
               selected_var = Category)
    } 
    if ("Level" %in% input$y_axis) {
      gender_balance_data <- graduates %>% 
        filter(Year >= input$year_slider[1], 
               Year <= input$year_slider[2]) %>% # filter for the years chosen by slider
        group_by(Level, Gender) %>%
        summarise(Headcount = sum(Headcount)) %>%
        group_by(Level) %>%
        mutate(percentage = Headcount/sum(Headcount),
               selected_var = forcats::fct_rev(Level)) # reverse the order of the Study level factor
    }
    return(gender_balance_data)
  })
  
  # gender_balance plot
  output$gender_balance <- renderPlot({ # note: gender_balance is the name of the plotOutput defined in the UI
    ggplot(gender_balance_data(), # need to use data from reactive function! (and not "graduates" df)
           aes(x = selected_var, y = percentage, group = Gender)) +
      geom_col(aes(fill = Gender)) +
      geom_text(aes(label = paste0(round(100 * percentage, 1), "%") ), position = position_stack(vjust = 0.5)) +
      theme_minimal() + 
      geom_abline(slope = 0, intercept = 0.5,  col = "#e9ecef", lty = 2) +
      coord_flip() + 
      labs(
        title = paste0("The Gender Balance by ", input$y_axis),
        subtitle = paste0("Academic years ",
                          input$year_slider[1], "/", as.numeric(str_sub(input$year_slider[1], start = 3L, end = 4)) + 1,
                          " - ",
                          input$year_slider[2], "/", as.numeric(str_sub(input$year_slider[2], start = 3L, end = 4)) + 1),
        x = "",
        y = "Gender Balance"
      ) +
      scale_fill_manual(values = my_palette[2:3]) +
      scale_y_continuous(labels = scales::percent, breaks = c(0.5)) + # keep only the 50%
      theme(
        text = element_text(size = 18, color = "#e9ecef"),
        axis.text = element_text(color = "#e9ecef"),
        plot.background = element_rect(fill = "#272B30", color = "#272B30"))
  })
  
  
  # ----- Everything needed for the second tab, time_trends:APC 
  
  # the data for the plot
  time_trends_data <- reactive({
    # if the y-axis var is headcount:
    if ("Headcount" %in% input$num_or_perc_cat) {
      if ("All Categories" %in% input$filter_line_cat) { # first all categories  
        time_trends_data <- graduates %>% 
          # filter(Category == "Engineering and Technology") %>% # no grouping for overall 
          group_by(Year, Gender) %>%
          summarise(Headcount = sum(Headcount)) %>%
          mutate(selected_var = Headcount)
      }
      for (cat in category_names) { # then a loop for each cagegory:
        if (cat %in% input$filter_line_cat) {
          time_trends_data <- graduates %>% 
            filter(Category == cat) %>% 
            group_by(Year, Gender) %>%
            summarise(Headcount = sum(Headcount)) %>%
            mutate(selected_var = Headcount)
        }
      }
    }
    
    # if the y-axis is percentages
    if ("Percentage" %in% input$num_or_perc_cat) {
      if ("All Categories" %in% input$filter_line_cat) { # first all categories  
        time_trends_data <- graduates %>% 
          # filter(Category == "Engineering and Technology") %>% # no grouping for overall 
          group_by(Year, Gender) %>%
          summarise(Headcount = sum(Headcount)) %>%
          group_by(Year) %>%
          mutate(percentage = Headcount/sum(Headcount)) %>%
          mutate(selected_var = percentage)
      }
      for (cat in category_names) { # then a loop for each cagegory:
        if (cat %in% input$filter_line_cat) {
          time_trends_data <- graduates %>% 
            filter(Category == cat) %>% 
            group_by(Year, Gender) %>%
            summarise(Headcount = sum(Headcount)) %>%
            group_by(Year) %>%
            mutate(percentage = Headcount/sum(Headcount)) %>%
            mutate(selected_var = percentage)
        }
      }
    }
    
    return(time_trends_data)
  })
  
  output$time_trends_APC <- renderPlot({
    p <- ggplot(time_trends_data(),
                aes(x = Year, y = selected_var, group = Gender)) +
      geom_line(aes(color = Gender), size = 2) +
      theme_minimal() +
      labs(
        title = "The Gender Balance Over Time",
        subtitle = input$filter_line_cat,
        x = "Academic Year", 
        y = input$num_or_perc_cat
      ) + 
      scale_color_manual(values = my_palette[2:3]) + 
      theme(
        text = element_text(size = 18, color = "#e9ecef"),
        axis.text = element_text(color = "#e9ecef"),
        plot.background = element_rect(fill = "#272B30", color = "#272B30"))    
    # change the scale of y-axis for percentages
    if ("Percentage" %in% input$num_or_perc_cat) {
      p <- p + 
        scale_y_continuous(labels = scales::percent_format(accuracy = 1L))
    }
    p # plot
    
  })
  
  
  # ----- Everything needed for the third tab, time_trends:Level 
  
  # the data for the plot
  time_trends_data_level <- reactive({
    # if the y-axis var is headcount:
    if ("Headcount" %in% input$num_or_perc_level) {
      if ("All Levels" %in% input$filter_line_level) { # first all categories  
        time_trends_data_level <- graduates %>% 
          # filter(Category == "Engineering and Technology") %>% # no grouping for overall 
          group_by(Year, Gender) %>%
          summarise(Headcount = sum(Headcount)) %>%
          mutate(selected_var = Headcount)
      }
      for (cat in level_names) { # then a loop for each cagegory:
        if (cat %in% input$filter_line_level) {
          time_trends_data_level <- graduates %>% 
            filter(Level == cat) %>% 
            group_by(Year, Gender) %>%
            summarise(Headcount = sum(Headcount)) %>%
            mutate(selected_var = Headcount)
        }
      }
    }
    
    # if the y-axis is percentages
    if ("Percentage" %in% input$num_or_perc_level) {
      if ("All Levels" %in% input$filter_line_level) { # first all categories  
        time_trends_data_level <- graduates %>% 
          # filter(Category == "Engineering and Technology") %>% # no grouping for overall 
          group_by(Year, Gender) %>%
          summarise(Headcount = sum(Headcount)) %>%
          group_by(Year) %>%
          mutate(percentage = Headcount/sum(Headcount)) %>%
          mutate(selected_var = percentage)
      }
      for (cat in level_names) { # then a loop for each cagegory:
        if (cat %in% input$filter_line_level) {
          time_trends_data_level <- graduates %>% 
            filter(Level == cat) %>% 
            group_by(Year, Gender) %>%
            summarise(Headcount = sum(Headcount)) %>%
            group_by(Year) %>%
            mutate(percentage = Headcount/sum(Headcount)) %>%
            mutate(selected_var = percentage)
        }
      }
    }
    
    return(time_trends_data_level)
  })
  
  output$time_trends_Level <- renderPlot({
    p <- ggplot(time_trends_data_level(),
                aes(x = Year, y = selected_var, group = Gender)) +
      geom_line(aes(color = Gender), size = 2) +
      theme_minimal() +
      labs(
        title = "The Gender Balance Over Time",
        subtitle = input$filter_line_level,
        x = "Academic Year", 
        y = input$num_or_perc_level
      ) + 
      scale_color_manual(values = my_palette[2:3]) + 
      theme(
        text = element_text(size = 18, color = "#e9ecef"),
        axis.text = element_text(color = "#e9ecef"),
        plot.background = element_rect(fill = "#272B30", color = "#272B30"))
    # change the scale of y-axis for percentages
    if ("Percentage" %in% input$num_or_perc_level) {
      p <- p + 
        scale_y_continuous(labels = scales::percent_format(accuracy = 1L))
    }
    p # plot
    
  })
  
  
  # ----- Everything needed for the fourth tab, some observations
  # (the plots are static, they do not take any inputs)
  output$imbalance_by_APC <- renderPlot({
    graduates %>%
      group_by(Category, Gender) %>%
      summarise(Headcount = sum(Headcount)) %>%
      group_by(Category) %>%
      mutate(percentage = Headcount/sum(Headcount)) %>%
      ggplot(
        aes(x = Category, y = percentage, group = Gender)
      ) +
      geom_col(aes(fill = Gender), position = "fill") +
      geom_text(aes(label = paste0(round(100 * percentage, 1), "%") ), position = position_stack(vjust = 0.5)) +
      theme_minimal() +
      geom_abline(slope = 0, intercept = 0.5,  col = "#e9ecef", lty = 2) +
      coord_flip() +
      labs(
        title = "The gender balance varies greatly by APC",
        x = "",
        y = "Gender Balance"
      ) +
      scale_fill_manual(values = my_palette[2:3]) +
      scale_y_continuous(labels = scales::percent, breaks = c(0.5)) + # keep only the 50%
      theme(
        text = element_text(size = 16, color = "#e9ecef"),
        axis.text = element_text(color = "#e9ecef"),
        plot.background = element_rect(fill = "#272B30", color = "#272B30"))  
    })
  
  output$engineering <- renderPlot({
    graduates %>% 
      filter(Category == "Engineering and Technology") %>%
      group_by(Year, Gender) %>%
      summarise(Headcount = sum(Headcount)) %>%
      ggplot(
        aes(x = Year, y = Headcount, group = Gender)
      ) +
      geom_line(aes(color = Gender), size = 2) +
      theme_minimal() +
      labs(
        title = "The gender imbalance is increasing in Engineering and Technology",
        subtitle = "The number of males graduates is increasing, whereas the number of females graduates is stable",
        x = "Academic Year", 
        y = "Number of Graduates"
      ) + 
      scale_color_manual(values = my_palette[2:3]) +
      theme(
        text = element_text(size = 16, color = "#e9ecef"),
        axis.text = element_text(color = "#e9ecef"),
        plot.background = element_rect(fill = "#272B30", color = "#272B30"))
    })
  
  output$leaky_pipeline <- renderPlot({
    graduates %>% 
      group_by(Level, Gender) %>%
      summarise(Headcount = sum(Headcount)) %>%
      group_by(Level) %>%
      mutate(percentage = Headcount/sum(Headcount)) %>%
      ggplot(
        aes(x = Level, y = percentage, group = Gender)
      ) +
      geom_col(aes(fill = Gender), position = "stack") +
      geom_text(aes(label = paste0(round(100 * percentage, 1), "%") ), position = position_stack(vjust = 0.5)) +
      geom_abline(slope = 0, intercept = 0.5,  col = "#e9ecef", lty = 2) + 
      theme_minimal() +
      labs(
        title = "The Research Postgraduate level is the only level\nwhere female graduates are in minority",
        x = "", 
        y = "Gender Balance"
      ) + 
      coord_flip() +
      scale_fill_manual(values = my_palette[2:3]) +
      scale_x_discrete(limits = rev(levels(graduates$Level))) +
      scale_y_continuous(labels = scales::percent, breaks = c(0.5)) + # keep only the 50%
      theme(
        text = element_text(size = 16, color = "#e9ecef"),
        axis.text = element_text(color = "#e9ecef"),
        plot.background = element_rect(fill = "#272B30", color = "#272B30"))  
    })
  
  # Close the server definition
})


##############
# Launch App #
##############

# generic line that launches the app
shinyApp(ui = ui, server = server)




