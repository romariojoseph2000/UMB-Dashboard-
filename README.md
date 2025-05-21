# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(scales)

# Define UI
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = tags$b("Pressure Check Study Dashboard")),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary Stats", tabName = "summary", icon = icon("tachometer-alt")),
      menuItem("Visualizations", tabName = "viz", icon = icon("chart-pie"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      body, .content-wrapper, .right-side {
        background-color: #f7f9fc;
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
      }
      .box {
        border-radius: 8px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.1);
      }
      .value-box {
        font-size: 22px !important;
        font-weight: 600 !important;
      }
      .small-box.bg-aqua {
        background-color: #00c0ef !important;
        color: white !important;
      }
      .small-box.bg-yellow {
        background-color: #f39c12 !important;
        color: white !important;
      }
      .small-box.bg-purple {
        background-color: #605ca8 !important;
        color: white !important;
      }
      .small-box.bg-green {
        background-color: #00a65a !important;
        color: white !important;
      }
      .box-title {
        font-weight: 700 !important;
        font-size: 18px !important;
        color: #333333;
      }
    "))),
    
    tabItems(
      tabItem(tabName = "summary",
              fluidRow(
                valueBox(204, "Total Screened Participants", icon = icon("users"), color = "aqua"),
                valueBox(2, "Languages Spoken", icon = icon("language"), color = "yellow"),
                valueBox("White & Black", "Most Prevalent Race/Ethnicity", icon = icon("user-circle"), color = "purple"),
                valueBox("18-25 years", "Most Screened Age Group", icon = icon("user"), color = "green")
              )
      ),
      
      tabItem(tabName = "viz",
              fluidRow(
                box(title = "Primary Language Distribution", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("langPlot", height = "350px")),
                box(title = "Blood Pressure Reading Frequency", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("bpPlot", height = "350px"))
              ),
              fluidRow(
                box(title = "Race Distribution", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("racePlot", height = "350px")),
                box(title = "UMB Unique Screenings Over Time", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("umbPlot", height = "350px"))
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Language Distribution Pie Chart
  output$langPlot <- renderPlot({
    lang_df <- data.frame(
      Language = factor(c("English", "Spanish"), levels = c("English", "Spanish")),
      Count = c(48, 2)
    )
    
    ggplot(lang_df, aes(x = "", y = Count, fill = Language)) +
      geom_col(color = "white") +
      geom_text(aes(label = Count), position = position_stack(vjust = 0.5), color = "white", size = 6) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c("#0073B7", "#E69F00")) +
      theme_void() +
      theme(
        legend.position = "right",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "#333333")
      ) +
      labs(title = "Primary Language Distribution", fill = "Language")
  })
  
  # BP Reading Frequency Bar Chart (with whole numbers only)
  output$bpPlot <- renderPlot({
    bp_df <- data.frame(
      Frequency = factor(c("Always", "Sometimes", "Never"), levels = c("Always", "Sometimes", "Never")),
      Count = c(9, 4, 3)  # Whole numbers only
    )
    
    ggplot(bp_df, aes(x = Frequency, y = Count, fill = Frequency)) +
      geom_bar(stat = "identity", color = "black", width = 0.7) +
      geom_text(aes(label = Count), vjust = -0.5, size = 5) +
      scale_fill_manual(values = c("#00a65a", "#f39c12", "#dd4b39")) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "#333333"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12)
      ) +
      labs(title = "Blood Pressure Reading Frequency",
           x = "Frequency",
           y = "Number of Participants")
  })
  
  # Race Distribution Bar Chart
  output$racePlot <- renderPlot({
    race_df <- data.frame(
      Race = factor(c("White", "Black or African American", "Asian", "Other (specify)", "Native Hawaiian/Other Pacific Islander"),
                    levels = c("White", "Black or African American", "Asian", "Other (specify)", "Native Hawaiian/Other Pacific Islander")),
      Count = c(7, 6, 5, 2, 1)
    )
    
    ggplot(race_df, aes(x = Race, y = Count, fill = Race)) +
      geom_bar(stat = "identity", color = "black", width = 0.7) +
      geom_text(aes(label = Count), vjust = -0.5, size = 5) +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "#333333"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 12)
      ) +
      labs(title = "Race Distribution",
           x = "Race/Ethnicity",
           y = "Number of Participants")
  })
  
  # UMB Unique Screenings Over Time Line Chart
  output$umbPlot <- renderPlot({
    umb_df <- data.frame(
      Date = as.Date(c(
        "2024-12-10", "2025-01-28", "2025-02-03", "2025-02-12", "2025-02-18", "2025-02-27",
        "2025-03-03", "2025-03-11", "2025-03-24", "2025-03-25", "2025-04-01", "2025-04-09",
        "2025-04-15", "2025-04-22", "2025-04-28", "2025-05-05", "2025-05-14"
      )),
      Count = c(28, 11, 23, 3, 11, 26, 8, 24, 1, 10, 7, 5, 18, 8, 9, 5, 7)
    )
    
    ggplot(umb_df, aes(x = Date, y = Count)) +
      geom_line(color = "#0073B7", linewidth = 1.5) +
      geom_point(size = 3, color = "#0073B7") +
      geom_text(aes(label = Count), vjust = -0.7, size = 4) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "#333333"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12)
      ) +
      labs(title = "UMB Unique Screenings Over Time",
           x = "Date",
           y = "Screenings")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




