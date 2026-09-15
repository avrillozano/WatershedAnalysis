library(shiny)
library(bslib)
library(tidyverse)
library(kableExtra)
library(ggridges)
library(lubridate)
options(scipen = 999)

lady_bird <- read.csv("lady_bird.csv")
shoal_creek <- read.csv("shoal_creek.csv")

NON_POISON_METALS <- c("ALUMINUM", "BORON", "COPPER", "IRON", "MANGANESE", "MOLYBDENUM", "SELENIUM", "SILVER", "STRONTIUM", "ZINC")
POISONOUS_METALS <- c("ANTIMONY", "ARSENIC", "BARIUM", "BERYLLIUM", "CADMIUM", "CHROMIUM", "LEAD", "MERCURY", "NICKEL", "THALLIUM")

preprocess_data <- function(df) {
  df <- df %>%
    mutate(RESULT = parse_number(as.character(RESULT))) %>%
    mutate(SAMPLE_DATE = as_date(mdy_hms(SAMPLE_DATE))) %>%
    select(WATERSHED, SITE_NAME, SAMPLE_DATE, PARAM_TYPE, PARAMETER, QUALIFIER, RESULT, UNIT, MEDIUM) %>%
    filter(PARAM_TYPE == "Metals") %>%
    mutate(
      RESULT = case_when(
        UNIT %in% c("MG/L", "MG/KG") ~ RESULT * 1000,
        TRUE ~ RESULT
      ),
      UNIT = case_when(
        UNIT %in% c("MG/L", "MG/KG", "UG/KG") ~ "UG/L",
        TRUE ~ UNIT
      )
    )
  
  df <- df %>%
    mutate(
      METALTYPE = case_when(
        PARAMETER %in% POISONOUS_METALS ~ "Poisonous",
        PARAMETER %in% NON_POISON_METALS ~ "Non-poison",
        TRUE ~ "Other"
      ),
      month = month(SAMPLE_DATE, label = TRUE)
    ) %>%
    filter(METALTYPE != "Other") %>%
    arrange(SAMPLE_DATE)
  
  return(df)
}

A_preprocessed <- preprocess_data(shoal_creek)
B_preprocessed <- preprocess_data(lady_bird)
my_data <- rbind(A_preprocessed, B_preprocessed)

poison <- filter(my_data, METALTYPE == "Poisonous")
non_poison <- filter(my_data, METALTYPE == "Non-poison")

shoal_creek_metals <- filter(my_data, WATERSHED == "Shoal Creek")
lady_bird_lake_metals <- filter(my_data, WATERSHED == "Lady Bird Lake")
shoal_poisonous_metals <- filter(shoal_creek_metals, METALTYPE == "Poisonous")
shoal_non_poison <- filter(shoal_creek_metals, METALTYPE == "Non-poison")
lady_poisonous_metals <- filter(lady_bird_lake_metals, METALTYPE == "Poisonous")
lady_non_poison <- filter(lady_bird_lake_metals, METALTYPE == "Non-poison")

ui <- fluidPage(
  
  titlePanel('Project 3: Investigating Toxic and Nontoxic Metals in Shoal Creek and Lady Bird Lake️'),
  h5(em("This project is meant to analyse the levels of both toxic and non-toxic metals across Shoal Creek and Lady Bird Lake across four variables:
  location, toxicity, metal type, and time. To obtain a univariate graph, select either a location, or a variable--not both. To get a multivariate graph, select both.
        ")),
  
  sidebarLayout(
    sidebarPanel(
      card(
        card_header("Locations to Analyze"),
        checkboxGroupInput(
          "mycheckGroup",
          "Select location(s) to analyze",
          choices = list("Lady Bird Lake" = "1", "Shoal Creek" = "2"),
          selected = "1"
        )
      ),
      
      card(
        card_header("Variables to Analyze"),
        checkboxGroupInput(
          "checkGroup",
          "Select metal group(s) to analyze",
          choices = list("Toxic metals" = "1", "Non-toxic metals" = "2"),
          selected = "1"
        )
      ),
      
      radioButtons("radio", label = h3("Analysis Type"),
                   choices = list("By Metal Type/Distribution" = "1", "Over Time" = "2"),
                   selected = "1"),
      checkboxInput("show_stats", "Display descriptive statistics", value = TRUE),
      
      card(
        card_header("Year Range"),
        sliderInput("slider1",
                    label = h3("Select date range"),
                    min = as.Date("1982-05-01"),
                    max = as.Date("2025-10-21"),
                    value = as.Date("2025-10-21"),
                    timeFormat = "%Y-%m-%d")
      ),
      
        
      
    ),
    
    mainPanel(
      plotOutput("myplot"),
      fluidRow(column(width = 10, verbatimTextOutput("mydata"))),
      uiOutput("image"),
      p(em("Wikimedia Commons. (n.d.). Lady Bird Lake in Austin, Texas."))
    )
  )
)


server <- function(input, output) {
  
  output$image <- renderUI({
    tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/0/09/AustinSkylineLouNeffPoint-Jun2010-a.JPG", width = "50%", height = "400px")
  })
  
  filtered_data_react <- reactive({
    
    # Check for empty selection and return empty data frame immediately
    if (length(input$checkGroup) == 0 && length(input$mycheckGroup) == 0) {
      return(my_data[0, ])
    }
    
    metal_filter <- c()
    if ("1" %in% input$checkGroup) {
      metal_filter <- c(metal_filter, "Poisonous")
    }
    if ("2" %in% input$checkGroup) {
      metal_filter <- c(metal_filter, "Non-poison")
    }
    
    # If no metals are selected, use the entire dataset (for counts/time plots of all metals)
    if (length(metal_filter) == 0) {
      df <- my_data
    } else {
      df <- filter(my_data, METALTYPE %in% metal_filter)
    }
    
    location_filter <- c()
    if ("1" %in% input$mycheckGroup) {
      location_filter <- c(location_filter, "Lady Bird Lake")
    }
    if ("2" %in% input$mycheckGroup) {
      location_filter <- c(location_filter, "Shoal Creek")
    }
    
    if (length(location_filter) > 0) {
      df <- filter(df, WATERSHED %in% location_filter)
    }
    
    df <- df %>%
      filter(between(SAMPLE_DATE, as.Date("1982-05-01"), as.Date(input$slider1)))
    
    return(df)
  })
  
  plot_logic <- function(data, metal_type_sel, location_sel, radio_sel, date_max) {
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    if (radio_sel == 1) {
      if (length(metal_type_sel) == 0) {
        
        title_location_scope <- if(length(location_sel) == 0) " in Shoal Creek and Lady Bird Lake" else if(all(c("1", "2") %in% location_sel)) " in Shoal Creek and Lady Bird Lake" else if("1" %in% location_sel) " in Lady Bird Lake" else " in Shoal Creek"
        title_base <- paste0("Total metal observations by metal", title_location_scope)
        
        p <- ggplot(data) + 
          geom_bar(aes(y = PARAMETER), fill = "orchid") + 
          labs(title = paste0(title_base, " through ", format(date_max, "%B %d, %Y")),
               x = "Total observations", y = "Metal")
        
        return(p)
      }
      
      if (length(location_sel) == 0) {
        title_base <- "Total metal observations by metal"
        if ("1" %in% metal_type_sel) {
          title_base <- ifelse("2" %in% metal_type_sel, "Total toxic and non-toxic metal observations by metal", "Total toxic metal observations by metal")
          fill_color <- ifelse("2" %in% metal_type_sel, "orchid", "maroon")
        } else if ("2" %in% metal_type_sel) {
          title_base <- "Total non-toxic metal observations by metal"
          fill_color <- "lightblue"
        } else {
          fill_color <- "orchid"
        }
        
        p <- ggplot(data) +
          geom_bar(aes(y = PARAMETER), fill = fill_color) +
          labs(title = paste0(title_base, " through ", format(date_max, "%B %d, %Y")),
               x = "Total observations", y = "Metal")
        return(p)
      }
      
      title_location <- if(all(c("1", "2") %in% location_sel)) "in Shoal Creek and Lady Bird Lake" else if("1" %in% location_sel) "in Lady Bird Lake" else "in Shoal Creek"
      title_metal <- if(all(c("1", "2") %in% metal_type_sel)) "Toxic and non-toxic metal measurements" else if("1" %in% metal_type_sel) "Toxic metal measurements" else "Non-toxic metal measurements"
      
      fill_option <- if (all(c("1", "2") %in% metal_type_sel)) "D" else if ("1" %in% metal_type_sel) "C" else "A"
      
      p <- ggplot(data, aes(x = RESULT, y = PARAMETER, fill = stat(y))) +
        geom_density_ridges_gradient() +
        scale_x_log10() +
        scale_fill_viridis_c(name = "Gradient Label", option = fill_option) +
        labs(title = paste0(title_metal, " by metal type ", title_location, " through ", format(date_max, "%B %d, %Y")),
             x = "Measurement (UG/L)", y = "Metal type") +
        theme(legend.position = "none")
      
      if (all(c("1", "2") %in% location_sel)) {
        p <- p + facet_wrap(~WATERSHED)
      }
      return(p)
      
    } else if (radio_sel == 2) {
      
      if (length(location_sel) == 0) {
        title_base <- "Total metal observations"
        fill_color <- "orchid"
        if ("1" %in% metal_type_sel) {
          title_base <- ifelse("2" %in% metal_type_sel, "Total toxic and non-toxic metal observations", "Total toxic metal observations")
          fill_color <- ifelse("2" %in% metal_type_sel, "orchid", "maroon")
        } else if ("2" %in% metal_type_sel) {
          title_base <- "Total non-toxic metal observations"
          fill_color <- "lightblue"
        }
        
        p <- ggplot(data) +
          geom_histogram(aes(x = year(SAMPLE_DATE)), fill = fill_color, binwidth = 1) +
          labs(title = paste0(title_base, " through ", format(date_max, "%B %d, %Y")),
               x = "Sample year", y = "Count")
        return(p)
      }
      
      title_location <- if(all(c("1", "2") %in% location_sel)) "in Shoal creek and Lady Bird Lake" else if("1" %in% location_sel) "in Lady Bird Lake" else "in Shoal Creek"
      title_metal <- if(all(c("1", "2") %in% metal_type_sel)) "Toxic and non-toxic metal measurements" else if("1" %in% metal_type_sel) "Toxic metal measurements" else "Non-toxic metal measurements"
      
      p <- ggplot(data, aes(x = SAMPLE_DATE, y = RESULT, color = WATERSHED)) +
        scale_y_log10() +
        labs(title = paste0(title_metal, " ", title_location, " through ", format(date_max, "%B %d, %Y")),
             x = "Year", y = "Measurement (UG/L)", color = "Location") +
        xlim(as.Date("1982-05-01"), date_max) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      # Use geom_line if specific metals are selected (or all are selected explicitly)
      if (length(metal_type_sel) > 0) {
        p <- p + geom_line()
      } else {
        p <- p + geom_point()
      }
      return(p)
    }
  }
  
  output$myplot <- renderPlot({
    plot_logic(
      data = filtered_data_react(),
      metal_type_sel = input$checkGroup,
      location_sel = input$mycheckGroup,
      radio_sel = input$radio,
      date_max = as.Date(input$slider1)
    )
  })
  
  output$mydata <- renderText({
        if (!input$show_stats) {
      return("")
    }
    
    data <- filtered_data_react()
    
    if (nrow(data) == 0) {
      return("")
    }
    
    location_sel <- input$mycheckGroup
    metal_type_sel <- input$checkGroup
    radio_sel <- input$radio
    
    if (radio_sel == 1) {
      if (length(location_sel) == 0) {
        if ("1" %in% metal_type_sel && "2" %in% metal_type_sel) {
          max_poison_metal <- str_to_title(poison$PARAMETER[which.max(poison$RESULT)])
          max_poison_val <- prettyNum(round(max(poison$RESULT), 2), big.mark = ",")
          max_non_metal <- str_to_title(non_poison$PARAMETER[which.max(non_poison$RESULT)])
          max_non_val <- prettyNum(round(max(non_poison$RESULT), 2), big.mark = ",")
          return(paste0("The **toxic metal** with the highest measurement was **", max_poison_metal,
                        "**, with levels of ", max_poison_val, " UG/L, and the **non-toxic metal** with the highest measurement was **",
                        max_non_metal, "**, with levels of ", max_non_val, " UG/L."))
        } else if ("1" %in% metal_type_sel) {
          max_poison_metal <- str_to_title(poison$PARAMETER[which.max(poison$RESULT)])
          max_poison_val <- prettyNum(round(max(poison$RESULT), 2), big.mark = ",")
          return(paste0("The toxic metal with the highest measurement was **", max_poison_metal,
                        "**, with levels of ", max_poison_val, " UG/L."))
        } else if ("2" %in% metal_type_sel) {
          max_non_metal <- str_to_title(non_poison$PARAMETER[which.max(non_poison$RESULT)])
          max_non_val <- prettyNum(round(max(non_poison$RESULT), 2), big.mark = ",")
          return(paste0("The non-toxic metal with the highest measurement was **", max_non_metal,
                        "**, with levels of ", max_non_val, " UG/L."))
        } else {
          tbl <- table(my_data$PARAMETER)
          return(paste0("The metal with the most observations was **", str_to_title(names(tbl)[which.max(tbl)]),
                        "** for Shoal Creek and Lady Bird Lake."))
        }
      } else if (all(c("1", "2") %in% location_sel)) {
        if (nrow(data) == 0) return("")
        median_shoal <- prettyNum(round(median(data$RESULT[data$WATERSHED == "Shoal Creek"], na.rm = TRUE), 2), big.mark = ",")
        median_lady <- prettyNum(round(median(data$RESULT[data$WATERSHED == "Lady Bird Lake"], na.rm = TRUE), 2), big.mark = ",")
        metal_label <- if (all(c("1", "2") %in% metal_type_sel)) "toxic and non-toxic metal" else if ("1" %in% metal_type_sel) "toxic metal" else "non-toxic metal"
        return(paste0("The median amount of ", metal_label, " observations was **", median_shoal,
                      "** UG/L for Shoal Creek, and **", median_lady, "** UG/L for Lady Bird Lake."))
      } else {
        if (nrow(data) == 0) return("")
        location_name <- unique(data$WATERSHED)
        if (length(metal_type_sel) > 0) {
          median_val <- prettyNum(round(median(data$RESULT, na.rm = TRUE), 2), big.mark = ",")
          metal_label <- if (all(c("1", "2") %in% metal_type_sel)) "toxic and non-toxic metal" else if ("1" %in% metal_type_sel) "toxic metal" else "non-toxic metal"
          return(paste0("The median amount of ", metal_label, " observations for **", location_name, "** was **", median_val, "** UG/L."))
        } else {
          tbl <- table(data$PARAMETER)
          return(paste0("The metal with the most observations was **", str_to_title(names(tbl)[which.max(tbl)]),
                        "** for **", location_name, "**."))
        }
      }
    }
    
    else if (radio_sel == 2) {
      if (length(location_sel) == 0) {
        if (nrow(data) == 0) return("")
        x <- data %>%
          mutate(year = year(SAMPLE_DATE)) %>%
          group_by(year) %>%
          summarise(n = n(), .groups = 'drop')
        
        max_year <- x$year[which.max(x$n)]
        metal_label <- if (all(c("1", "2") %in% metal_type_sel)) "toxic and non-toxic metal" else if ("1" %in% metal_type_sel) "toxic metal" else "non-toxic metal"
        return(paste0("The year with the most ", metal_label, " observations was **", max_year, "**."))
        
      } else if (all(c("1", "2") %in% location_sel)) {
        if (nrow(data) == 0) return("")
        
        max_shoal <- prettyNum(round(max(data$RESULT[data$WATERSHED == "Shoal Creek"], na.rm = TRUE), 2), big.mark = ",")
        max_lady <- prettyNum(round(max(data$RESULT[data$WATERSHED == "Lady Bird Lake"], na.rm = TRUE), 2), big.mark = ",")
        metal_label <- if (all(c("1", "2") %in% metal_type_sel)) "toxic and non-toxic metal" else if ("1" %in% metal_type_sel) "toxic metal" else "non-toxic metal"
        
        if (length(metal_type_sel) > 0) {
          return(paste0("The highest measurement among ", metal_label, " observations was **", max_shoal,
                        "** UG/L for Shoal Creek, and **", max_lady, "** UG/L for Lady Bird Lake."))
        } else {
          max_result <- max(data$RESULT, na.rm = TRUE)
          max_metal <- str_to_title(data$PARAMETER[which.max(data$RESULT)])
          return(paste0("The metal with the highest measurement was **", max_metal,
                        "**, with levels of ", prettyNum(round(max_result, 2), big.mark = ","), " UG/L."))
        }
      } else {
        if (nrow(data) == 0) return("")
        location_name <- unique(data$WATERSHED)
        max_result <- max(data$RESULT, na.rm = TRUE)
        max_val <- prettyNum(round(max_result, 2), big.mark = ",")
        
        if (length(metal_type_sel) > 0) {
          metal_label <- if (all(c("1", "2") %in% metal_type_sel)) "toxic and non-toxic metal" else if ("1" %in% metal_type_sel) "toxic metal" else "non-toxic metal"
          return(paste0("The highest measurement among ", metal_label, " observations for **", location_name, "** was **", max_val, "** UG/L."))
        } else {
          max_metal <- str_to_title(data$PARAMETER[which.max(data$RESULT)])
          return(paste0("The metal with the highest measurement was **", max_metal,
                        "**, with levels of ", max_val, " UG/L."))
        }
      }
    }
  })
}

shinyApp(ui = ui, server = server)