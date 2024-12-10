# Load packages used by the app. Install missing packages, if needed.
library(shiny)
library(bslib)
library(thematic)
library(tidyverse)
library(gitlink)
library(shiny)
library(glue)
library(ggplot2)
library(ggrepel)
# Load necessary libraries
library(dplyr)
library(readr)
library(reshape2)
library(forcats) 
library(scales)

# Read data from a CSV file and perform data preprocessing
expansions <- read_csv("data/ADMIRAL.csv") |>
  mutate(
    Institutions = factor(`Institution or Study Abbreviation`, levels = c("CHOM", "E15", "SMILES", "1000 GEN", "CCLS", "CCRLP", "WashU", "COG and PEC", "BCM/UTSW", "VUMC", "CHOA", "CHOP", "UAB", "MSK", "RP/DFCI", "DCFI 05-001", "SJLIFE", "TARGET")),
    Type = factor(Type, levels = c("CONTRIBUTING INSTITUTIONS", "CONTRIBUTING STUDIES", "CONTROLS", "OTHER CONTRIBUTING STUDIES")),
    `Case Controls` = factor(`Case Controls`, levels = c("Cases", "Cases & Controls", "Controls")),
    `Genotyped or Analyzed in UMN` = factor(`Genotyped or Analyzed in UMN`, levels = c("Yes", "No")),
    `Sample Status` = factor(`Sample Status`, levels = c("Samples in progress", "Collecting sample list", "Completed", "Not applicable", "On hold")),
    `Clinical Data Status` = factor(`Clinical Data Status`, levels = c("To request", "Completed", "In progress", "Not applicable", "On hold")),
    `Sample Types` = factor(`Sample Types`, levels = c("No samples yet", "Extracted", "Unextracted", "Unextracted and extracted", "Not applicable", "On hold")),
    `Received any samples` = factor(`Received any samples`, levels = c("No samples yet", "Completed", "Some Samples Received", "Not applicable", "On hold"))
  )|>
  ungroup()


# Compute expansion rates by type, `Case Controls`, `Sample Status`, `Clinical Data Status`, `Received any samples`
expansion_groups <- expansions |>
  group_by(Institutions) |>
  summarize(
    `Existing Case Samples Total` = sum(`Existing Case Samples`),
    `Anticipated Case Samples Total` = sum(`Anticipated Case Samples`),
    `Case Samples Total` = sum(`Case Samples`),
    `Control Samples Total` = sum(`Control Samples`),
    `Parent Samples Total` = sum(`Parent Samples`),
    `Total Samples` = sum(`All Samples`),
    `Cases Needed to Reach Existing Anticipated Case Samples Total` = sum(`Cases Needed to Reach Existing + Anticipated Case Samples`),
    n = n()
  ) |>
  ungroup()
colnames(expansion_groups)

# Define lists for propensity, contract and industry choices

Institutionss <-c("CHOM",
                                                                                             "E15",
                                                                                             "SMILES",
                                                                                             "1000 GEN",
                                                                                             "CCLS",
                                                                                             "CCRLP",
                                                                                             "WashU",
                                                                                             "COG and PEC",
                                                                                             "BCM/UTSW",
                                                                                             "VUMC ",
                                                                                             "CHOA",
                                                                                             "CHOP",
                                                                                             "UAB",
                                                                                             "MSK",
                                                                                             "RP/DFCI",
                                                                                             "DCFI 05-001",
                                                                                             "SJLIFE",
                                                                                             "TARGET")

Types<-c("CONTRIBUTING INSTITUTIONS", "CONTRIBUTING STUDIES", "CONTROLS", "OTHER CONTRIBUTING STUDIES")

CaseControls<-c("Cases","Cases & Controls","Controls")

# Set the default theme for ggplot2 plots
ggplot2::theme_set(ggplot2::theme_minimal())

# Apply the CSS used by the Shiny app to the ggplot2 plots
thematic_shiny()



# Define the Shiny UI layout
ui <- page_sidebar(
  
  # Set CSS theme
  theme = bs_theme(bootswatch = "cyborg",
                   bg = "#7A0019",
                   fg = "#FFCC33",
                   success ="#FFCC33"),
  
  # Add title
  title = "ADMIRAL Bioriospecimen and Clinical Data requisition and retrieval",
  
  # Add sidebar elements
  sidebar = sidebar(title = "Select a segment of data to view",
                    class ="bg-secondary",
                    selectInput("Institutions", "Select Institutions or Studoes", choices = Institutionss, selected = "", multiple  = TRUE),
                    selectInput("Type", "Select Types", choices = Types, selected = "", multiple  = TRUE),
                    selectInput("Case Controls", "Select Case Controls", choices = CaseControls, selected = "", multiple  = TRUE),
                    "This app illstrated the Bioriospecimen and Clinical Data requisition and retrieval"),
  
  
  # Layout non-sidebar elements
  layout_columns(card(card_header("Total Samples Received"),
                      plotOutput("bar1")),
                 
                 card(card_header("Total Samples Received by Types"),
                      plotOutput("bar2")),
                 
                 card(card_header("Sample Status"),
                      plotOutput("pie1")),
                 
                 card(card_header("Clinical Data Status"),
                      plotOutput("pie2")),
                 
                 card(card_header("Received any samples"),
                      plotOutput("pie3")),
                 
                 card(card_header("Summary of the Sample Retrieval Status"),
                      tableOutput("table")),
                 col_widths = c(8, 4, 4, 4, 4, 12),
                 row_heights = c(3.1, 2.6, 2.5))
)

# Define the Shiny server function
server <- function(input, output) {
  
  # Provide default values for selections
  selected_Institutionss <- reactive({
    if (is.null(input$Institutions)) Institutionss else input$Institutions
  })
  
  selected_Types <- reactive({
    if (is.null(input$Type)) Types else input$Type
  })
  
  selected_CaseControls <- reactive({
    if (is.null(input$`Case Controls`)) CaseControls else input$`Case Controls`
  })
  
  # Filter data against selections
  filtered_expansions <- reactive({
    expansions %>%
      filter(
        Institutions %in% selected_Institutionss(),
        Type %in% selected_Types(),
        `Case Controls` %in% selected_CaseControls()
      )
  })
  
  # Compute conversions by month (uncomment if needed)
  # conversions <- reactive({
  #   filtered_expansions() %>%
  #     mutate(date = floor_date(date, unit = "month")) %>%
  #     group_by(date, evaluation) %>%
  #     summarize(n = sum(outcome == "Won")) %>%
  #     ungroup()
  # })
  
  # Retrieve conversion rates for selected groups
  groups <- reactive({
    expansion_groups %>%
      filter(
        Institutions %in% selected_Institutionss(),
        Type %in% selected_Types(),
        `Case Controls` %in% selected_CaseControls()
      )
  })

  
  # Render bar plot for conversion rates by subgroup
  output$bar1 <- renderPlot({
    ggplot(expansion_groups, aes(x = Institutions, y = `Total Samples`, fill = Institutions)) +
      geom_col() +
      geom_text(aes(label = `Total Samples`), vjust = -0.5, size = 4, fontface = "bold") + guides(fill = "none") +
      theme(axis.title = element_blank(), axis.text.y = element_text(face="bold",size=12),
            axis.text.x = element_text(angle = 40,hjust =1,face="bold",size=12)) +
      scale_y_continuous(limits=c(0,800))
  })
  
  # Render bar plot for conversion rates by subgroup
  
  output$bar2 <- renderPlot({
    
    sampletypes <- expansions %>% 
      dplyr::select(Institutions, `Case Samples`, `Control Samples`, `Parent Samples`)  
    
    data_frame_relapse3 <- melt(sampletypes, id.vars = c("Institutions"), variable.name = "Translocation", 
                                value.name = "Score") 
    
    data_frame_relapse_all3 <- aggregate(data_frame_relapse3$Score, 
                                         list(data_frame_relapse3$Translocation), 
                                         FUN = sum) %>% 
      group_by(Group.1) %>% 
      ungroup() %>% 
      mutate(Group.1_g = factor(
        ifelse(Group.1 == "Case Samples", 1,
               ifelse(Group.1 == "Control Samples", 2, 
                      ifelse(Group.1 == "Parent Samples", 3, NA))), 
        levels = c(1, 2, 3), labels = c("Case Samples", "Control Samples", "Parent Samples")))
    
    ggplot(data_frame_relapse_all3, aes(x = Group.1_g, y = x, fill = x)) +  # Convert x to factor
      geom_bar(stat = "identity", width = 0.8) +
      geom_text(aes(label = x), position = position_fill(vjust = 900), size = 3.5, color = "black", fontface = "bold") +
      theme_classic() +
      theme(axis.text.x = element_text(hjust = 1)) +
      scale_x_discrete(labels = scales::label_wrap(30)) +
      labs(x = "", y = "", fill = "", title = "") +
      theme(legend.position = "none", legend.text = element_text(face = "bold", size = 10),
            legend.title = element_text(face = "bold"), plot.title = element_text(size = 12, face = "bold"),
            axis.title = element_text(face = "bold"),  
            axis.text.y = element_text(face = "bold", size = 12),
            axis.text.x = element_text(face = "bold", size = 11)) +
      theme(text = element_text(family = "sans")) + coord_flip()
    
    
  })
  
  
  # Render pie chart for Sample Status
  output$pie1 <- renderPlot({
    sample_status_counts <- expansions %>%
      group_by(`Sample Status`) %>%
      summarize(Count = n()) %>%
      mutate(amount = Count / sum(Count)) 

    
      ggplot(sample_status_counts, aes(x = "", y = Count, fill = `Sample Status`)) +
      geom_bar(stat = "identity", width = 1, color = "White") +
      coord_polar("y", start = 0) +
      theme_void() +
      # geom_text_repel(aes(label = glue("{Count} ({scales::percent(amount)})"), y = pos),size = 4, color = "black") +
      theme(legend.title = element_blank(),
            legend.text = element_text(face = "bold", size = 12, color = "white"))
  })
  
  # Render pie chart for Clinical Data Status
  output$pie2 <- renderPlot({
    clinical_data_counts <- expansions %>%
      group_by(`Clinical Data Status`) %>%
      summarize(Count = n()) %>%
      mutate(amount = Count / sum(Count))  
  
      ggplot(clinical_data_counts, aes(x = "", y = Count, fill = `Clinical Data Status`)) +
      geom_bar(stat = "identity", width = 1, color = "White") +
      coord_polar("y", start = 0) +
      theme_void() +
      # geom_text_repel(aes(label = glue("{Count} ({scales::percent(amount)})"), y = Count),size = 4, color = "black") +
      theme(legend.title = element_blank(),
            legend.text = element_text(face = "bold", size = 12, color = "white"))
  })
  
  # Render pie chart for Received any Samples
  
  output$pie3 <- renderPlot({
    received_samples_counts <- expansions %>%
      group_by(`Received any samples`) %>%
      summarize(Count = n()) %>%
      mutate(amount = Count / sum(Count)) 
    
    ggplot(received_samples_counts, aes(x = "", y = Count, fill = `Received any samples`)) +
      geom_bar(stat = "identity", width = 1, color = "White") +
      coord_polar("y", start = 0) +
      theme_void() +
      # geom_text_repel(aes(label = glue("{Count} ({scales::percent(amount)})"), y = Count),size = 4, color = "black") +
      theme(legend.title = element_blank(),
            legend.text = element_text(face = "bold", size = 12, color = "white"))
  })
  
  
  
  # Render table for selected data
  output$table <- renderTable({
    expansion_groups %>% 
      select(Institutions, `Existing Case Samples Total`, `Anticipated Case Samples Total`, `Case Samples Total`, `Control Samples Total`, `Parent Samples Total`, `Total Samples`)
  }, digits = 0)
}

# Create the Shiny app
shinyApp(ui = ui, server = server)

