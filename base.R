#' @name create_pie_chart
#' @title Create a Pie Chart from Categorical Data
#' 
#' 
#' @description Generates a pie chart from a categorical variable in a dataset, grouping small categories if needed.
#' 
#' @param data A `data.frame` containing the categorical data.
#' @param variable_col The column name in `data` representing the categorical variable.
#' @param title_col An optional title for the pie chart (default is `variable_col`).
#' @param max_categories Maximum number of categories to display before grouping smaller ones into "Others" (default is 10).
#' @param palette An optional color palette for the chart; if `NULL`, a default palette is used.
#' 
#' @import ggplot2 
#' @import dplyr 
#' @import grDevices colorRampPalette
#' 
#' @return A `ggplot` object representing the pie chart.
#' 
#' 
#' 
#' @examples
#' data <- data.frame(category = sample(LETTERS[1:5], 100, replace = TRUE))
#' create_pie_chart(data, variable_col = "category")
#' 
#' @import dplyr
#' @import ggplot2
#' 
#' 
create_pie_chart <- function(data, variable_col, title_col = variable_col, max_categories = 10, palette = NULL) {
  # Input validation
  if (is.null(data) || nrow(data) == 0) {
    stop("Input data is empty or NULL.")
  }
  if (!variable_col %in% colnames(data)) {
    stop(paste("Column", variable_col, "not found in the data."))
  }
  
  # Rename the target column for easier processing
  data <- data %>% rename(variable = all_of(variable_col))
  
  # Group and summarize the data
  categorical_data <- data %>%
    group_by(variable) %>%
    summarise(n = n(), .groups = 'drop')
  
  # If there are more categories than `max_categories`, group the smallest ones into "Others"
  if (nrow(categorical_data) > max_categories) {
    val_cutoff <- categorical_data %>%
      arrange(desc(n)) %>%
      slice(max_categories) %>%
      pull(n)
    
    cat_main <- categorical_data %>% filter(n >= val_cutoff)
    cat_others <- categorical_data %>%
      filter(n < val_cutoff) %>%
      summarise(variable = "Others", n = sum(n))
    
    categorical_data <- bind_rows(cat_main, cat_others)
  }
  
  # Calculate percentages
  categorical_data <- categorical_data %>%
    mutate(p = (n / sum(n)) * 100) %>%
    mutate(across(where(is.numeric), ~ round(., 2)))
  
  # Define a default color palette if none is provided
  if (is.null(palette)) {
    palette <- colorRampPalette(c("#1488CA", "#00A19A", "#FFED99"))(nrow(categorical_data))
  }
  
  # Create the pie chart
  pie_chart <- ggplot(categorical_data, aes(x = "", y = p, fill = variable)) +
    geom_bar(stat = "identity", width = 1, show.legend = TRUE) +
    geom_label(
      aes(label = paste0(p, "%")),
      position = position_stack(vjust = 0.5),
      alpha = 0.1,
      show.legend = FALSE
    ) +
    coord_polar(theta = "y") +
    theme_minimal() +
    scale_fill_manual(values = palette) +
    labs(
      title = paste("Pie-chart of", title_col),
      fill = title_col,
      x = NULL,
      y = NULL
    ) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      strip.background = element_blank(),
      axis.text = element_text(size = 14, face = "bold"),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 18, face = "bold")
    )
  
  return(pie_chart)
}

#' @name plot_map
#' @title Plot a Map Highlighting a Selected Country
#' 
#' @description This function generates a map highlighting a selected country or all European countries.
#' If "Europa" is selected, all European countries are highlighted.
#' The function corrects "Czech Republic" to "Czechia" for consistency with the dataset.
#' 
#' @param world A `sf` object representing the world map.
#' @param selected_country A string specifying the country to highlight. If "Europa", all European countries are highlighted.
#' 
#' @import ggplot2 
#' @import sf st_crs
#' 
#' @return A `ggplot` object representing the highlighted map.
#' 
#' @examples
#' library(sf)
#' world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
#' plot_map(world, "Italy")
#' plot_map(world, "Europa")
#' 
#' @import ggplot2
#' @import sf
#' 
plot_map <- function(world, selected_country) {
   # Correct "Czech Republic" to "Czechia"
   selected_country <- ifelse(selected_country == "Czech Republic", "Czechia", selected_country)
   
   # Color only the selected country
   world$color <- ifelse(world$name == selected_country, "#003D39", "#FFED99")
   
   
   # Plot
   ggplot(data = world) +
     geom_sf(aes(fill = color), color = "grey70") +
     scale_fill_identity() +
     theme_dark() +
     coord_sf(crs = st_crs(4326), xlim = c(-10, 40), ylim = c(35, 70)) +  
     theme(panel.background = element_rect(fill = 'white'),
           panel.grid.major = element_line(linewidth = 0.1, color = 'white'),
           legend.position = "none",
           axis.text = element_text(size = 12),
           axis.title = element_text(size = 18, face = "bold"),
           plot.title = element_text(hjust = 0.5)) + 
     labs(title = selected_country)
 }

#' @name generate_heatmap
#' @title Generate a Heatmap of Pearson Correlation Coefficients
#' 
#' @description Creates a heatmap displaying the Pearson correlation coefficient matrix 
#' from a given dataset containing numeric variables.
#' 
#' @param data A `data.frame` containing numerical variables.
#' @param title A character string specifying the title of the heatmap (default: "Heatmap of Pearson correlation coefficient matrix").
#' 
#' @import dplyr 
#' @import ggplot2 
#' @import reshape2 melt
#' 
#' @return A `ggplot` object representing the correlation heatmap.
#' If the input data is `NULL` or contains insufficient numerical data, the function returns `NULL`.
#' 
#' @examples 
#' # Example dataset
#' data <- mtcars
#' generate_heatmap(data, title = "Correlation Heatmap of mtcars")
#' 
#' @import tidyverse
#' @import reshape2
#' 
#' 
generate_heatmap <- function(data, title = "Heatmap of Pearson correlation coefficient matrix") {
  if (is.null(data)) {
    return(NULL)
  }
  
  numeric_data <- data %>%
    select_if(is.numeric) %>%
    select(!matches("Ads|Id|plot|n_specie")) %>%
    drop_na()
  
  if (nrow(numeric_data) <= 1) {
    return(NULL)  # Return NULL if there's not enough data for a heatmap
  }
  
  # Calculate correlation matrix
  cor_data <- cor(numeric_data, use = "complete.obs")
  
  # Get upper triangle of the correlation matrix
  get_upper_tri <- function(cormat) {
    cormat[lower.tri(cormat)] <- NA
    return(cormat)
  }
  
  upper_tri <- get_upper_tri(cor_data)
  cor_data_long <- melt(upper_tri, na.rm = TRUE)
  
  # Create heatmap
  heatmap <- ggplot(cor_data_long, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "#1488CA", mid = "#FFED99", high = "#da275d",
      midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Pearson\nCorrelation \n"
    ) +
    theme_minimal() +
    labs(title = title) +
    xlab("") + ylab("") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      strip.background = element_blank(),
      axis.text = element_text(size = 14, face = "bold"),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 16, face = "bold")
    ) +
    coord_fixed()
  
  return(heatmap)
}

#' @name generate_plot
#' @title Boxplot or Bar Plot of Numeric Variables
#' 
#' @description Generates a boxplot for multiple numeric variables or a bar plot if only one row of data is available.
#' 
#' @param data A `data.frame` containing numeric variables.
#' @param plot_title A `character` string specifying the title of the plot (default is "Boxplot of Numeric variables").
#' @param bg_color A `character` string specifying the background color of the plot (default is "#003D39").
#' 
#' @import dplyr 
#' @import tidyr 
#' @import ggplot2 
#' 
#' @return A `ggplot` object representing either a boxplot or a bar plot, depending on the number of data rows.
#' 
#' @examples 
#' sample_data <- data.frame(A = rnorm(100), B = rnorm(100))
#' generate_plot(sample_data, plot_title = "Sample Boxplot")
#' 
#' @import ggplot2
#' 
#' 
generate_plot <- function(data, plot_title = "Boxplot of Numeric variables", bg_color = "#003D39") {
  if (is.null(data)) {
    return(NULL)
  }
  
  numeric_data <- data %>%
    select_if(is.numeric) %>%
    select(!matches("Ads|Id|plot")) %>%
    drop_na()
  
  if (nrow(numeric_data) == 0) {
    return(NULL)  # Return NULL if no numeric data is available
  }
  
  if (nrow(numeric_data) == 1) {
    res_melted <- melt(numeric_data)
    plot <- ggplot(res_melted, aes(x = variable, y = value, fill = variable)) +
      geom_bar(stat = 'identity', fill = "#00A19A", show.legend = FALSE) +
      geom_label(aes(label = round(value, 1)), vjust = 0.5, hjust = 0.5, size = 5, fill = "white") +
      coord_flip() +
      theme_classic() +
      labs(x = "Variables\n", y = "\nValues") +
      theme(
        axis.text = element_text(size = 14),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
  } else {
    plot <- numeric_data %>%
      gather(key = "variable", value = "value") %>%
      ggplot(aes(x = variable, y = value)) +
      geom_boxplot(width = 0.1, fill = "#FFED99", alpha = 0.9) +
      facet_wrap(~variable, scales = "free", ncol = 4) +
      labs(title = plot_title) +
      theme_classic() +
      theme(
        strip.background = element_blank(),
        axis.text = element_text(size = 16, face = "bold"),
        strip.text = element_text(size = 16, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      xlab('\nVariables') +
      ylab('Values\n')
  }
  
  return(plot)
}


#' @name generate_pie_charts
#' @title Generate Pie Charts for Categorical Data
#' 
#' @description Creates pie charts for each categorical variable in the provided dataset.
#' It automatically converts character columns to factors and excludes unwanted columns.
#' 
#' @param data A `data.frame` containing categorical variables.
#' 
#' @import dplyr 
#' @import ggplot2 
#' @import ggpubr ggarrange
#' 
#' @return A ggplot object or a combined plot of multiple pie charts.
#' If no categorical variables are found, the function returns `NULL`.
#' 
#' @examples
#' df <- data.frame(
#'   Species = c("Oak", "Beech", "Oak", "Pine", "Beech"),
#'   Site = c("A", "B", "A", "C", "B")
#' )
#' 
#' generate_pie_charts(df)
#' 
#' @import ggplot2
#' @import ggpubr
#' 
#' 
generate_pie_charts <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }
  
  # Convert character columns to factors and select categorical columns
  categorical_data <- data %>%
    mutate_if(is.character, as.factor) %>%
    select_if(is.factor) %>%
    select(!matches("Ads|Id|plot"))
  
  if (ncol(categorical_data) == 0) {
    return(NULL)  # Return NULL if there are no categorical columns
  }
  
  # Generate pie charts for each categorical column
  pie_charts_list <- lapply(colnames(categorical_data), function(col_name) {
    create_pie_chart(data = categorical_data, variable_col = col_name, title_col = col_name)
  })
  
  # Combine pie charts into a single plot
  if (length(pie_charts_list) == 1) {
    return(pie_charts_list[[1]])
  } else {
    return(ggarrange(plotlist = pie_charts_list, ncol = 1))
  }
}

#' @name generate_report
#' @title Generate a Report from Processed Data
#' 
#' @description This function generates a PDF report based on a specified template, using processed data and visualization outputs.
#' The function selects figures based on the chosen report type (mitigation or adaptation).
#' 
#' @param file The output filename for the generated report (e.g., "report.pdf").
#' @param type A string indicating the type of report: `"mitigation"` or `"adaptation"`.
#' @param data_processing_function A function that processes data and returns a list containing at least two elements (data tables).
#' @param vals A list containing visualization objects (e.g., maps, heatmaps, pie charts, boxplots).
#' @param template_path The path to the RMarkdown template used for generating the report (default is `"template.Rmd"`).
#' 
#' @import rmarkdown render
#' @import utils file.copy
#' 
#' @return The function generates a PDF report and saves it to the specified `file` path.
#' 
#' @examples
#' \dontrun{
#' generate_report(
#'   file = "output.pdf",
#'   type = "mitigation",
#'   data_processing_function = my_data_processing_function,
#'   vals = list(
#'     map1 = my_map,
#'     heatmap_1 = my_heatmap,
#'     pie_charts_1 = my_pie_charts,
#'     bar_boxplot_1 = my_boxplot
#'   ),
#'   template_path = "template.Rmd"
#' )
#' }
#' 
#' 
#' 

generate_report <- function(file, type, data_processing_function, vals, template_path = "template.Rmd") {
  # Check if the template file exists
  if (!file.exists(template_path)) {
    stop("Report template file not found.")
  }
  
  # Copy the template to a temporary directory
  tempReport <- file.path(tempdir(), basename(template_path))
  file.copy(template_path, tempReport, overwrite = TRUE)
  
  # Process the data
  results <- data_processing_function()
  if (is.null(results) || length(results) < 2) {
    stop("Data processing returned NULL or insufficient data. Cannot generate report.")
  }
  
  # Extract data
  data1 <- results[[1]]
  data2 <- results[[2]]
  
  # Determine which values to use based on type
  if (type == "mitigation") {
    fig_map <- vals$map1
    fig_heatmap <- vals$heatmap_1
    fig_pie_charts <- vals$pie_charts_1
    fig_bar_boxplot <- vals$bar_boxplot_1
  } else if (type == "adaptation") {
    fig_map <- vals$map2
    fig_heatmap <- vals$heatmap_2
    fig_pie_charts <- NULL  # Not used in adaptation
    fig_bar_boxplot <- vals$bar_boxplot_2
  } else {
    stop("Invalid report type.")
  }
  
  # Debugging output
  cat("Generating report for type:", type, "\n")
  cat("Map included:", !is.null(fig_map), "\n")
  cat("Heatmap included:", !is.null(fig_heatmap), "\n")
  cat("Pie Charts included:", !is.null(fig_pie_charts), "\n")
  cat("Bar/Boxplot included:", !is.null(fig_bar_boxplot), "\n")
  
  # Create a list of parameters, excluding NULL values
  report_params <- list(
    fig_map = fig_map,
    dt_table = as.data.frame(data1),
    dt_table_ad = as.data.frame(data2),
    type = type,
    fig_heatmap = fig_heatmap,
    fig_bar_boxplot = fig_bar_boxplot
  )
  
  # Only add pie charts if it's not NULL
  if (!is.null(fig_pie_charts)) {
    report_params$fig_pie_charts <- fig_pie_charts
  }
  
  # Render the report
  rmarkdown::render(
    tempReport,
    output_format = "pdf_document",
    output_file = file,
    params = report_params,
    envir = new.env(parent = globalenv()),
    clean = FALSE,
    encoding = "utf-8"
  )
}




#' @name DataProcessingFunctions
#' @title Helper Functions for Data Processing in Shiny App
#' 
#' @description A set of functions to load, subset, and rename columns in datasets, designed for use in a Shiny application.
#' 
#' @param file_path The file path to the dataset (Excel format expected).
#' @param data A dataframe containing the data to be processed.
#' @param columns A character vector specifying the columns to retain in the subset.
#' @param new_names A character vector with new column names (must match the number of columns in `data`).
#' 
#' @import openxlsx 
#' 
#' @return The functions return processed data:
#' 
#' - `load_data()`: Returns a dataframe loaded from an Excel file.
#' - `subset_data()`: Returns a dataframe with only the specified columns.
#' - `rename_columns()`: Returns a dataframe with renamed columns.
#' 
#' @examples 
#' # Load data
#' data <- load_data("C:/Users/calvi/OneDrive/Desktop/App_Forwards/app/Data/Example.xlsx")
#' 
#' # Subset specific columns
#' subset <- subset_data(data, c("TreeDiameter", "TreeHeight"))
#' 
#' # Rename columns
#' renamed_data <- rename_columns(subset, c("NewName1", "NewName2"))
#' 
#' @import shiny
#' @import dplyr
#' 
# Function to load data from file
load_data <- function(file_path) {
  req(file_path)
  data <- openxlsx::read.xlsx(file_path)
  return(data)
}

# Function to subset data based on selected columns
subset_data <- function(data, columns) {
  data_subset <- data[, columns, drop = FALSE]
  return(data_subset)
}

# Function to rename columns
rename_columns <- function(data, new_names) {
  if (length(new_names) != ncol(data)) {
    stop("Length of new_names must match number of columns in data")
  }
  colnames(data) <- new_names
  return(data)
}

#' @name process_growing_stock
#' @title Process Growing Stock Model
#'
#' @description Processes growing stock data by subsetting relevant columns and applying the stem volume calculator.
#'
#' @param data A `data.frame` containing forest inventory data.
#' @param input A list containing column names and parameters required for processing.
#'
#' @return A processed dataset with calculated stem volume estimates.
#'
#' @import dplyr 
#'
#' @examples
#' data <- data.frame(
#'   ForManInt = c(1, 2, 1),
#'   plot = c("A", "B", "C"),
#'   th = c(15, 20, 25),
#'   dbh = c(30, 40, 50),
#'   specie = c("Fagus", "Quercus", "Pinus")
#' )
#'
#' input <- list(
#'   I1_ForManInt_col = "ForManInt",
#'   I1_plot_col = "plot",
#'   I1_height_col = "th",
#'   I1_dbh_col = "dbh",
#'   I1_specie_col = "specie",
#'   I1_Provide_ForManInt = "some_value",
#'   I1_Provide_plot = "some_value",
#'   I1_plot_size_col = 500
#' )
#'
#' results <- process_growing_stock(data, input)
#' 
#' @import ForestStandMetrics
#' 
# Function to process GrowingStock model
#
process_growing_stock <- function(data, input) {
  columns <- c(input$I1_ForManInt_col, input$I1_plot_col, input$I1_height_col, input$I1_dbh_col, input$I1_specie_col)
  data_subset <- subset_data(data, columns)
  data_subset <- rename_columns(data_subset, c("ForManInt", "plot", "th", "dbh", "specie"))
  
  results <- Apply_StemVolumeCalculator(
    data_subset, input$I1_Provide_ForManInt, "ForManInt", input$I1_Provide_plot,
    plot_col = "plot", plot_area = input$I1_plot_size_col, dbh_col = "dbh", th_col = "th", specie_col = "specie"
  )
  return(results)
}

#' @name process_carbon_stock
#' @title Process Carbon Stock Model
#'
#' @description Processes carbon stock data by subsetting relevant columns and applying the carbon stock calculator.
#'
#' @param data A `data.frame` containing forest inventory data.
#' @param input A list containing column names and parameters required for processing.
#'
#' @return A processed dataset with estimated carbon stock values.
#'
#' @import dplyr 
#'
#' @examples
#' data <- data.frame(
#'   ForManInt = c(1, 2, 1),
#'   plot = c("A", "B", "C"),
#'   dom_col = c(10, 15, 20),
#'   vol_m3_ha = c(200, 300, 400)
#' )
#'
#' input <- list(
#'   I2_ForManInt_col = "ForManInt",
#'   I2_plot_col = "plot",
#'   I2_dom_col = "dom_col",
#'   I2_vol_col = "vol_m3_ha",
#'   I2_Provide_ForManInt = "some_value",
#'   I2_Provide_plot = "some_value"
#' )
#'
#' results <- process_carbon_stock(data, input)
#'
#' 

# Function to process CarbonStock model
process_carbon_stock <- function(data, input) {
  columns <- c(input$I2_ForManInt_col, input$I2_plot_col, input$I2_dom_col, input$I2_vol_col)
  data_subset <- subset_data(data, columns)
  data_subset <- rename_columns(data_subset, c("ForManInt", "plot", "dom_col", "vol_m3_ha"))
  
  results <- Apply_CarbonStockCalculator(
    data_subset, input$I2_Provide_ForManInt, "ForManInt", input$I2_Provide_plot, "plot", "dom_col", "vol_m3_ha"
  )
  return(results)
}

#' @name process_lying_deadwood
#' @title Process Lying Deadwood Data
#'
#' @description Processes lying deadwood data by subsetting relevant columns and applying the deadwood estimation model.
#'
#' @param data A `data.frame` containing forest inventory data.
#' @param input A list containing column names and parameters required for processing.
#'
#' @return A processed dataset with estimated lying deadwood values.
#'
#' @importFrom dplyr 
#'
#' @examples
#' data <- data.frame(
#'   ForManInt = c(1, 2, 1),
#'   plot = c("A", "B", "C"),
#'   L_tot_col = c(10, 15, 20),
#'   Dhalf_col = c(0.3, 0.4, 0.5),
#'   TH_tot_col = c(1.5, 2.0, 2.5),
#'   DBH_col = c(25, 30, 35)
#' )
#'
#' input <- list(
#'   I3_ForManInt_col = "ForManInt",
#'   I3_plot_col = "plot",
#'   I3_L_tot_col = "L_tot_col",
#'   I3_Dhalf_col = "Dhalf_col",
#'   I3_TH_tot_col = "TH_tot_col",
#'   I3_DBH_col = "DBH_col",
#'   I3_Provide_ForManInt = "some_value",
#'   I3_Provide_plot = "some_value",
#'   I3_LDT_CWD_option = "some_option",
#'   I3_plot_size_col = "some_value"
#' )
#'
#' results <- process_lying_deadwood(data, input)
#'
#' 
#' 
process_lying_deadwood <- function(data, input) {
  columns <- c(input$I3_ForManInt_col, input$I3_plot_col, input$I3_L_tot_col, input$I3_Dhalf_col, input$I3_TH_tot_col, input$I3_DBH_col)
  data_subset <- subset_data(data, columns)
  data_subset <- rename_columns(data_subset, c("ForManInt", "plot", "L_tot_col", "Dhalf_col", "TH_tot_col", "DBH_col"))
  
  results <- Apply_LyingDeadwood(
    data_subset, input$I3_Provide_ForManInt, "ForManInt", input$I3_Provide_plot, "plot", 
    input$I3_LDT_CWD_option, "L_tot_col", "Dhalf_col", "TH_tot_col", "DBH_col", input$I3_plot_size_col
  )
  return(results)
}

#' @name process_standing_deadwood
#' @title Process Standing Deadwood Data
#'
#' @description Processes standing deadwood data by subsetting relevant columns and applying the deadwood estimation model.
#'
#' @param data A `data.frame` containing forest inventory data.
#' @param input A list containing column names and parameters required for processing.
#'
#' @return A processed dataset with estimated standing deadwood values.
#'
#' @import dplyr
#'
#' @examples
#' data <- data.frame(
#'   ForManInt = c(1, 2, 1),
#'   plot = c("A", "B", "C"),
#'   L_tot_col = c(10, 15, 20),
#'   Dhalf_col = c(0.3, 0.4, 0.5),
#'   TH_tot_col = c(1.5, 2.0, 2.5),
#'   DBH_col = c(25, 30, 35)
#' )
#'
#' input <- list(
#'   I4_ForManInt_col = "ForManInt",
#'   I4_plot_col = "plot",
#'   I4_L_tot_col = "L_tot_col",
#'   I4_Dhalf_col = "Dhalf_col",
#'   I4_TH_tot_col = "TH_tot_col",
#'   I4_DBH_col = "DBH_col",
#'   I4_Provide_ForManInt = "some_value",
#'   I4_Provide_plot = "some_value",
#'   I4_SDT_SNAG_option = "some_option",
#'   I4_plot_size_col = "some_value"
#' )
#'
#' results <- process_standing_deadwood(data, input)
#'
#' 

process_standing_deadwood <- function(data, input) {
  columns <- c(input$I4_ForManInt_col, input$I4_plot_col, input$I4_L_tot_col, input$I4_Dhalf_col, input$I4_TH_tot_col, input$I4_DBH_col)
  data_subset <- subset_data(data, columns)
  data_subset <- rename_columns(data_subset, c("ForManInt", "plot", "L_tot_col", "Dhalf_col", "TH_tot_col", "DBH_col"))
  
  results <- Apply_StandingDeadwood_MP_BEST(
    data_subset, input$I4_Provide_ForManInt, "ForManInt", input$I4_Provide_plot, "plot", 
    input$I4_SDT_SNAG_option, "L_tot_col", "Dhalf_col", "TH_tot_col", "DBH_col", input$I4_plot_size_col
  )
  return(results)
}

#' @name process_all_deadwood
#' @title Process All Deadwood Data
#'
#' @description Processes deadwood data by subsetting relevant columns and applying the deadwood estimation model.
#'
#' @param data A `data.frame` containing forest inventory data.
#' @param input A list containing column names and parameters required for processing.
#'
#' @return A processed dataset with estimated deadwood values.
#'
#' @import dplyr 
#'
#' @examples
#' data <- data.frame(
#'   ForManInt = c(1, 2, 1),
#'   plot = c("A", "B", "C"),
#'   L_tot_col = c(10, 15, 20),
#'   Dmax_col = c(0.5, 0.6, 0.7),
#'   Dmin_col = c(0.3, 0.4, 0.5)
#' )
#'
#' input <- list(
#'   I5_ForManInt_col = "ForManInt",
#'   I5_plot_col = "plot",
#'   I5_L_col = "L_tot_col",
#'   I5_Dmax_col = "Dmax_col",
#'   I5_Dmin_col = "Dmin_col",
#'   I5_Provide_ForManInt = "some_value",
#'   I5_Provide_plot = "some_value",
#'   I5_plot_size_col = "some_value"
#' )
#'
#' results <- process_all_deadwood(data, input)
#'
#' 

process_all_deadwood <- function(data, input) {
  columns <- c(input$I5_ForManInt_col, input$I5_plot_col, input$I5_L_col, input$I5_Dmax_col, input$I5_Dmin_col)
  data_subset <- subset_data(data, columns)
  data_subset <- rename_columns(data_subset, c("ForManInt", "plot", "L_tot_col", "Dmax_col", "Dmin_col"))
  
  results <- Apply_All_Deadwood(
    data_subset, input$I5_Provide_ForManInt, "ForManInt", input$I5_Provide_plot, "plot", 
    "L_tot_col", "Dmax_col", "Dmin_col", input$I5_plot_size_col
  )
  return(results)
}

#' @name process_forest_diversity
#' @title Forest Structural and Species Diversity Analysis
#'
#' @description Processes forest inventory data to assess structural and species diversity.
#'
#' @param data A `data.frame` containing forest inventory data.
#' @param input A list containing column names and parameters required for processing.
#'
#' @return A processed dataset with calculated forest diversity metrics.
#'
#' @import dplyr
#'
#' @examples
#' data <- data.frame(
#'   ForManInt = c(1, 2, 1),
#'   plot = c("A", "B", "C"),
#'   th = c(15, 20, 10),
#'   dbh = c(30, 25, 40),
#'   specie = c("Fagus", "Quercus", "Acer")
#' )
#'
#' input <- list(
#'   I51_ForManInt_col = "ForManInt",
#'   I51_plot_col = "plot",
#'   I51_height_col = "th",
#'   I51_dbh_col = "dbh",
#'   I51_specie_col = "specie",
#'   I51_Provide_ForManInt = "some_value",
#'   I51_Provide_plot = "some_value",
#'   I51_plot_size_col = "some_value"
#' )
#'
#' results <- process_forest_diversity(data, input)
#'
#' 

process_forest_diversity <- function(data, input) {
  columns <- c(input$I51_ForManInt_col, input$I51_plot_col, input$I51_height_col, input$I51_dbh_col, input$I51_specie_col)
  data_subset <- subset_data(data, columns)
  data_subset <- rename_columns(data_subset, c("ForManInt", "plot", "th", "dbh", "specie"))
  
  results <- Apply_ForStrSpecDiv(
    data_subset, input$I51_Provide_ForManInt, "ForManInt", input$I51_Provide_plot, "plot", 
    "dbh", "th", "specie", input$I51_plot_size_col
  )
  return(results)
}

#-------------------------------------------------------------------------------------
# End
#-------------------------------------------------------------------------------------
#' @name UI for CSF-Ind
#' @title UI for Climate Smart Forestry Indicator Generator
#'
#' @description This function defines the user interface (UI) for the Climate Smart Forestry (CSF) Indicator Generator.
#' It provides an interactive dashboard for processing adaptation and mitigation pillars.
#'
#' @param data A `data.frame` containing forest inventory data.
#' @param input A list containing column names and parameters required for processing.
#'
#' @return A processed dataset with calculated forest diversity metrics.
#'
#' @import dplyr 
#' @import shiny shinydashboard bslib DT ggplot2 GGally openxlsx ggpubr tidyverse rmarkdown gridExtra 
#' @import shinyscreenshot rnaturalearth sf tinytex kableExtra reshape2 vegan DescTools ForestStandMetrics
#-----------------------------------------------------------------------------
#                   Library
#-----------------------------------------------------------------------------
# List of required packages
required_packages <- c("shiny", "shinydashboard", "bslib", "DT", "ggplot2", "GGally", "openxlsx", 
                       "ggpubr", "tidyverse", "rmarkdown", "gridExtra", "shinyscreenshot", 
                       "rnaturalearth", "sf", "tinytex", "kableExtra", "reshape2", "vegan", 
                       "DescTools", "ForestStandMetrics")

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)

# Load the libraries
lapply(required_packages, library, character.only = TRUE)

options(shiny.maxRequestSize=100*(1024*1024))
#-----------------------------------------------------------------------------
# Countries
#-----------------------------------------------------------------------------
european_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", 
                        "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", 
                        "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", 
                        "Luxembourg", "Malta", "Netherlands", "Norway", "Poland", 
                        "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", 
                        "Switzerland", "United Kingdom")  # Add/adjust as needed
#-----------------------------------------------------------------------------
# Ui
#-----------------------------------------------------------------------------
ui <- dashboardPage(
  # Shiny App UI data
  dashboardHeader(title = div(
    style = "font-size: 24px; font-family: 'Brandon Grotesque'; font-weight: bold;",
    HTML(paste0(
      '<span style="color: #6FBC85;" >CSF-Ind</span>'#, 
    ))
  )),
  dashboardSidebar(
    div(style = "display: flex; align-items: center; justify-content: flex-start; padding: 10px;",
        a(href = "#Home",
          img(src = "https://www3.unimol.it/assets/images/unimol/images/header/unimol_on.svg", height = "80px", style = "margin-right: 10px; margin-left: 10px;")
        ),
        a(href = "#Home",
          img(src = "https://forwards-project.eu/wp-content/themes/forwards_theme/images/FWD-LOGO-RGB_MAIN_COLOR.svg", height = "80px", style = "margin-left: 5px;")
        )
    ),
    sidebarMenu(
      tags$div(
        style = "padding: 12px; font-size: 16px;font-family: 'Brandon Grotesque'; font-weight: bold;color:#003D39;",
        "CSF concept"
      ),
      menuItem(paste0("Mitigation"), tabName =paste0("Mitigation"), icon = icon("ok", lib = "glyphicon")),
      menuItem(paste0("Adaptation"), tabName =paste0("Adaptation"), icon = icon("ok", lib = "glyphicon")),
      menuItem(paste0("Social_Dimension"), tabName =paste0("Social_Dimension"), icon = icon("ok", lib = "glyphicon")),
      menuItem(paste0("CSF_Composite_Assessment"), tabName =paste0("CSF_Composite_Performance_Assessment"), icon = icon("menu-hamburger", lib = "glyphicon"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/css_app.css")
    ),
    # *********************************
    #  First Page
    # *********************************
    tabItems(
      tabItem(tabName = "Mitigation",
              fluidPage(
                titlePanel(
                  div(
                    style = "font-size: 30px; font-family: 'Brandon Grotesque'; font-weight: bold;",
                    HTML(paste0(
                      '<span style="color:#f9f6f6;">CSF - Mitigation Pillar </span>'
                    ))
                  )
                ),
                sidebarLayout(
                  sidebarPanel(
                    div(
                      style = "margin-bottom: -10px;", 
                      fileInput("file","Upload Excel file (.xlsx)",buttonLabel = "Choose File")
                    ),
                    selectInput("country", "Select EU country or Europe for analysis", choices = european_countries, selected = "Italy"),
                    conditionalPanel(
                      condition = "input.country == 'Italy'",
                      selectInput("model_1", label = "Select CSF indicator to analyse", 
                                  choices = c("1.1_GrowingStock","1.2_CarbonStock", "1.3_LyingDeadwood","1.4_StandingDeadwood","1.5_AllDeadwood"), 
                                  selected = "1.2_CarbonStock"),
                      # *********************************************************
                      # ************************  1  ****************************
                      # *********************************************************
                      conditionalPanel(
                        condition = "input.model_1 == '1.1_GrowingStock'",
                        hr(),
                        radioButtons(
                          inputId = "I1_Provide_ForManInt",
                          label = "Will you provide ForManInt information?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I1_Provide_ForManInt == 'Yes'",
                          selectInput("I1_ForManInt_col", "Select ForManInt Column", "")
                        ),
                        hr(),
                        radioButtons(
                          inputId = "I1_Provide_plot",
                          label = "Will you conduct a multi-plot analysis?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I1_Provide_plot == 'Yes'",
                          selectInput("I1_plot_col", "Select Plot ID Column", "")
                        ),
                        hr(),
                        sliderInput("I1_plot_size_col", "Configurate Plot Area (m2):", min = 200, max = 1000, value = 530, step = 10),
                        verbatimTextOutput("Slider1Out"),
                        selectInput("I1_height_col", "Select Tree Height (m) Column", ""),
                        selectInput("I1_dbh_col", "Select Tree Diameter at 1.3m Column", ""),
                        selectInput("I1_specie_col", "Select Tree Species Column", ""),
                        hr()
                      ),
                      # *********************************************************
                      # ************************  2  ****************************
                      # *********************************************************
                      conditionalPanel(
                        condition = "input.model_1 == '1.2_CarbonStock'",
                        hr(),
                        radioButtons(
                          inputId = "I2_Provide_ForManInt",
                          label = "Will you provide ForManInt information?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I2_Provide_ForManInt == 'Yes'",
                          selectInput("I2_ForManInt_col", "Select ForManInt Column", "")
                        ),
                        hr(),
                        radioButtons(
                          inputId = "I2_Provide_plot",
                          label = "Will you conduct a multi-plot analysis?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I2_Provide_plot == 'Yes'",
                          selectInput("I2_plot_col", "Select Plot ID Column", "")
                        ),
                        hr(),
                        selectInput("I2_dom_col", "Select Dominant Tree Species Column",choices = NULL),
                        selectInput("I2_vol_col", "Select Stand Volume (m3/ha) Column", ""),
                        hr()
                      ),
                      # *********************************************************
                      # ************************   3 ****************************
                      # *********************************************************
                      conditionalPanel(
                        condition = "input.model_1 == '1.3_LyingDeadwood'",
                        hr(),
                        radioButtons(
                          inputId = "I3_Provide_ForManInt",
                          label = "Will you provide ForManInt information?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I3_Provide_ForManInt == 'Yes'",
                          selectInput("I3_ForManInt_col", "Select ForManInt Column", "")
                        ),
                        hr(),
                        radioButtons(
                          inputId = "I3_Provide_plot",
                          label = "Will you conduct a multi-plot analysis?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I3_Provide_plot == 'Yes'",
                          selectInput("I3_plot_col", "Select Plot ID Column", "")
                        ),
                        hr(),
                        radioButtons(
                          inputId = "I3_LDT_CWD_option",
                          label = "Will you calculate LDT or CWD ?",
                          choices = list("ldt" = "ldt", "cwd" = "cwd"),
                          selected = "ldt"
                        ),
                        conditionalPanel(
                          condition = "input.I3_LDT_CWD_option == 'ldt'",
                          selectInput("I3_TH_tot_col", "Select Tree Height (m) Column", ""),
                          selectInput("I3_DBH_col", "Select Tree Diameter at 1.3m (cm) Column", "")
                        ),
                        conditionalPanel(
                          condition = "input.I3_LDT_CWD_option == 'cwd'",
                          selectInput("I3_L_tot_col", "Select Height or Length (m) Column", ""),
                          selectInput("I3_Dhalf_col", "Select Diameter at half-length (cm) Column", "")
                        ),
                        sliderInput("I3_plot_size_col", "Configurate Plot Area (m2):", min = 200, max = 1000, value = 530, step = 10),
                        verbatimTextOutput("Slider1Out")
                      ),
                      # *********************************************************
                      # ************************   4 ****************************
                      # *********************************************************
                      conditionalPanel(
                        condition = "input.model_1 == '1.4_StandingDeadwood'",
                        hr(),
                        radioButtons(
                          inputId = "I4_Provide_ForManInt",
                          label = "Will you provide ForManInt information?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I4_Provide_ForManInt == 'Yes'",
                          selectInput("I4_ForManInt_col", "Select ForManInt Column", "")
                        ),
                        hr(),
                        radioButtons(
                          inputId = "I4_Provide_plot",
                          label = "Will you conduct a multi-plot analysis?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I4_Provide_plot == 'Yes'",
                          selectInput("I4_plot_col", "Select Plot ID Column", "")
                        ),
                        hr(),
                        radioButtons(
                          inputId = "I4_SDT_SNAG_option",
                          label = "Will you calculate LDT or CWD ?",
                          choices = list("sdt" = "sdt", "snag" = "snag"),
                          selected = "sdt"
                        ),
                        conditionalPanel(
                          condition = "input.I4_SDT_SNAG_option == 'sdt'",
                          selectInput("I4_TH_tot_col", "Select Tree Height (m) Column", ""),
                          selectInput("I4_DBH_col", "Select Tree Diameter at 1.3m (cm) Column", "")
                        ),
                        conditionalPanel(
                          condition = "input.I4_SDT_SNAG_option == 'snag'",
                          selectInput("I4_L_tot_col", "Select Height or Length  (m) Column", ""),
                          selectInput("I4_Dhalf_col", "Select Diameter at half-length (cm) Column", "")
                        ),
                        sliderInput("I4_plot_size_col", "Configurate Plot Area (m2):", min = 200, max = 1000, value = 530, step = 10),
                        verbatimTextOutput("Slider1Out")
                      ),
                      # *********************************************************
                      # ************************   4 ****************************
                      # *********************************************************
                      conditionalPanel(
                        condition = "input.model_1 == '1.5_AllDeadwood'",
                        hr(),
                        radioButtons(
                          inputId = "I5_Provide_ForManInt",
                          label = "Will you provide ForManInt information?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I5_Provide_ForManInt == 'Yes'",
                          selectInput("I5_ForManInt_col", "Select ForManInt Column", "")
                        ),
                        hr(),
                        radioButtons(
                          inputId = "I5_Provide_plot",
                          label = "Will you conduct a multi-plot analysis?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I5_Provide_plot == 'Yes'",
                          selectInput("I5_plot_col", "Select Plot ID Column", "")
                        ),
                        hr(),
                        sliderInput("I5_plot_size_col", "Configurate Plot Area (m2):", min = 200, max = 1000, value = 530, step = 10),
                        verbatimTextOutput("Slider1Out"),
                        selectInput("I5_L_col", "Select Height or Length (m) Column", ""),
                        selectInput("I5_Dmax_col", "Select Maximum Diameter (cm) Column", ""),
                        selectInput("I5_Dmin_col", "Select Minimum Diameter  (cm) Column", ""),
                      )
                    ),
                    
                    conditionalPanel(
                      condition = "input.country == 'Italy'",
                      downloadButton("downloadData_Miti_ForMan", "Download Table ForManInt (.xlsx)", icon("download"), 
                                     style = "color: #fff; background-color: #00A19A; border-color: white;width:100%; font-size:40"),
                      downloadButton("downloadData_Miti_Plot", "Download Table Plot (.xlsx)", icon("download"), 
                                     style = "color: #fff; background-color: #00A19A; border-color: white;width:100%; font-size:40"),
                      downloadButton("downloadReportPDF_1", "Download Report (.pdf)", icon("download"),
                                     style = "color: #fff; background-color: #00A19A; border-color: white;width:100%; font-size:40"),
                      actionButton("reset", "Reset", icon("rotate-left"),
                                   style = "color: #fff; background-color: #da275d; border-color: white;width:100%; font-size:40")
                      
                    ),
                    style = "width: 300px;"
                  ),
                  mainPanel(
                    fluidRow(
                      box(title = "Background", color= "olive", solidHeader = TRUE,
                          collapsible = TRUE,background = "olive",imageOutput("CSF1", height = 200)),
                      box(title = "Country Selected", color= "olive", solidHeader = TRUE,
                          collapsible = TRUE,background = "olive",plotOutput("mapEU1", height = 200))
                    ) ,
                    DTOutput("data_summary_1"),
                    plotOutput("bar_boxplot_1"),
                    conditionalPanel(
                      condition = "output.piechartAvailable_1 == true",
                      plotOutput("pie_charts_1",height = "500px")
                    ),
                    conditionalPanel(
                      condition = "output.heatmapAvailable_1 == true",
                      plotOutput("heatmap_1",height = "500px")
                    )
                  )
                )
              )
      ),
      # *********************************
      #  Second Page
      # *********************************
      tabItem(tabName = "Adaptation",
              fluidPage(
                titlePanel(
                  div(
                    style = "font-size: 30px; font-family: 'Brandon Grotesque'; font-weight: bold;",
                    HTML(paste0(
                      '<span style="color:#f9f6f6;">CSF - Adaptation Pillar </span>'
                    ))
                  )
                ),
                sidebarLayout(
                  sidebarPanel(
                    div(
                      style = "margin-bottom: -10px;", 
                      fileInput("file", "Upload Excel file (.xlsx)",buttonLabel = "Choose File")
                    ),
                    selectInput("country", "Select country for report location", choices = european_countries, selected = "Italy"),
                    conditionalPanel(
                      condition = "input.country == 'Italy'",
                      selectInput("model_2", label = "Select CSF indicator to analyse", 
                                  choices = c("2.1_ForestDiversity","2.2_AdvancedForestDiversity"), 
                                  selected = "2.1_ForestDiversity"),
                      conditionalPanel(
                        condition = "input.model_2 == '2.1_ForestDiversity'",
                        hr(),
                        radioButtons(
                          inputId = "I51_Provide_ForManInt",
                          label = "Will you provide ForManInt information?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I51_Provide_ForManInt == 'Yes'",
                          selectInput("I51_ForManInt_col", "Select ForManInt Column", "")
                        ),
                        hr(),
                        radioButtons(
                          inputId = "I51_Provide_plot",
                          label = "Will you conduct a multi-plot analysis?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I51_Provide_plot == 'Yes'",
                          selectInput("I51_plot_col", "Select Plot ID Column", "")
                        ),
                        hr(),
                        sliderInput("I51_plot_size_col", "Configurate Plot Area (m2):", min = 200, max = 1000, value = 530, step = 10),
                        verbatimTextOutput("Slider1Out"),
                        selectInput("I51_height_col", "Select Tree Height (m) Column", ""),
                        selectInput("I51_dbh_col", "Select Tree Diameter at 1.3m(cm) Column", ""),
                        selectInput("I51_specie_col", "Select Tree Species Column", ""),
                        hr()
                      ),
                      conditionalPanel(
                        condition = "input.model_2 == '2.2_AdvancedForestDiversity'",
                        hr(),
                        selectInput("I6_ForManInt_col", "Select ForManInt Column", ""),
                        hr(),
                        sliderInput("I6_plot_size_col", "Configurate Plot Area (m2):",min = 200, max = 1000, value = 530, step = 10),
                        verbatimTextOutput("Slider1Out"),
                        selectInput("I6_plot_col", "Select Plot ID Column", choices = NULL),
                        selectInput("I6_dbh_col", "Select Tree Diameter (cm) Column", ""),
                        selectInput("I6_height_col", "Select Tree Height (m) Column", ""),
                        selectInput("I6_ba_col", "Select Basal area (m2) Column", ""),
                        selectInput("I6_specie_col", "Select Tree Species Column", ""),
                        hr()
                      )
                    ),
                    conditionalPanel(
                      condition = "input.country == 'Italy'",
                      downloadButton("downloadData_Adap_ForMan", "Download Table (.xlsx)", icon("download"), 
                                     style = "color: #fff; background-color: #00A19A; border-color: white;width:100%; font-size:40"),
                      downloadButton("downloadData_Adap_Plot", "Download Table (.xlsx)", icon("download"), 
                                     style = "color: #fff; background-color: #00A19A; border-color: white;width:100%; font-size:40"),
                      downloadButton("downloadReportPDF_2", "Download Report (.pdf)", icon("download"),
                                     style = "color: #fff; background-color: #00A19A; border-color: white;width:100%; font-size:40"),
                      actionButton("reset", "Reset", icon("rotate-left"),
                                   style = "color: #fff; background-color: #da275d; border-color: white;width:100%; font-size:40")
                    ),
                    style = "width: 300px;"
                  ),
                  mainPanel(#style = "padding: 0px 0px; margin-top:0em margin-left:0em",
                    fluidRow(
                      box(title = "Background", color= "olive", solidHeader = TRUE,
                          collapsible = TRUE,background = "olive",imageOutput("CSF2", height = 200)),
                      box(title = "Country Selected", color= "olive", solidHeader = TRUE,
                          collapsible = TRUE,background = "olive",plotOutput("mapEU2", height = 200))
                    ) ,
                    #
                    #
                    DTOutput("data_summary_2"),
                    plotOutput("bar_boxplot_2"),
                    conditionalPanel(
                      condition = "output.heatmapAvailable_2 == true",
                      plotOutput("heatmap_2",height = "500px")
                    )
                  )
                )
              )
      ),
      # *********************************
      #  Third Page
      # *********************************
      tabItem(tabName = "Social_Dimension",
              fluidPage(
                titlePanel(
                  div(
                    style = "font-size: 30px; font-family: 'Brandon Grotesque'; font-weight: bold;",
                    HTML(paste0(
                      '<span style="color:#f9f6f6;">CSF - Social Dimension Pillar </span>'
                    ))
                  )
                ))
      ),
      # *********************************
      #  Fourth
      # *********************************
      tabItem(tabName = "CSF_Composite_Performance_Assessment",
              fluidPage(
                titlePanel(
                  div(
                    style = "font-size: 30px; font-family: 'Brandon Grotesque'; font-weight: bold;",
                    HTML(paste0(
                      '<span style="color:#f9f6f6;">CSF Composite Performance Assessment</span>'
                    ))
                  )
                ))
      )
      # *********************************
      #  To be continue
      # *********************************
    )
  )
) 

#' @name Server for CSF-Ind
#' @title Server for Climate Smart Forestry Indicator Generator
#'
#' @description This function defines the server  for the Climate Smart Forestry (CSF) Indicator Generator.
#' It provides an interactive dashboard for processing adaptation and mitigation pillars.
#'
#' @param data A `data.frame` containing forest inventory data.
#' @param input A list containing column names and parameters required for processing.
#'
#' @return A processed dataset with calculated forest diversity metrics.
#'
#' @import dplyr 
#' @import shiny shinydashboard bslib DT ggplot2 GGally openxlsx ggpubr tidyverse rmarkdown gridExtra 
#' @import shinyscreenshot rnaturalearth sf tinytex kableExtra reshape2 vegan DescTools ForestStandMetrics

#-----------------------------------------------------------------------------
# Server
#-----------------------------------------------------------------------------

server <- function(input, output, session) {
  # **************************************
  #        CSF logo
  # ************************************** 
  output$CSF1 <- renderImage({
    # Return a list containing the filename
    list(src = 'C:/Users/calvi/OneDrive/Desktop/App_Forwards/R/Logo/CSF-ciclo1.png',
         width = "100%",
         height = "100%",
         alt = "Chart of good stuff")
  }, deleteFile = FALSE)
  output$CSF2 <- renderImage({
    # Return a list containing the filename
    list(src = 'C:/Users/calvi/OneDrive/Desktop/App_Forwards/R/Logo/CSF-ciclo2.png',
         width = "100%",
         height = "100%",
         alt = "Chart of good stuff")
  }, deleteFile = FALSE)
  # **************************************
  #        EU map
  # ************************************** 
  output$mapEU1 <- renderPlot({
    selected_country <- input$country
    world <- ne_countries(scale = 50, returnclass = "sf")
    map1 <- plot_map(world, selected_country)
    vals$map1 <- map1
    print(map1)
  })
  output$mapEU2 <- renderPlot({
    selected_country <- input$country
    world <- ne_countries(scale = 50, returnclass = "sf")
    map2 <- plot_map(world, selected_country)
    vals$map2 <- map2
    print(map2)
  })
  # **************************************
  #        Main Reactive Functions
  # **************************************
  
  data_processing_1 <- reactive({
    data <- load_data(input$file$datapath)
    
    switch(input$model_1,
           "1.1_GrowingStock" = process_growing_stock(data, input),
           "1.2_CarbonStock" = process_carbon_stock(data, input),
           "1.3_LyingDeadwood" = process_lying_deadwood(data, input),
           "1.4_StandingDeadwood" = process_standing_deadwood(data, input),
           "1.5_AllDeadwood" = process_all_deadwood(data, input)
    )
  })
  data_processing_2 <- reactive({
    data <- load_data(input$file$datapath)
    
    if (input$model_2 == "2.1_ForestDiversity") {
      process_forest_diversity(data, input)
    }
  })
  # **************************************
  #        Show 10 rows in Shiny
  # ************************************** 
  vals <- reactiveValues()
  output$data_summary_1 <- renderDT({
    data <- data_processing_1()
    if (is.null(data)) {
      return(NULL)
    }
    # 
    vals$data_summary_1 <- datatable(data[[2]],rownames = FALSE,
                                     options = list(
                                       columnDefs = list(list(className = 'dt-center', targets = '_all'))
                                     ))
    vals$data_summary_1
  })
  output$data_summary_2 <- renderDT({
    data <- data_processing_2()
    if (is.null(data)) {
      return(NULL)
    }
    # 
    vals$data_summary_2 <- datatable(data[[2]],rownames = FALSE,
                                     options = list(
                                       columnDefs = list(list(className = 'dt-center', targets = '_all'))
                                     ))
    vals$data_summary_2
  })
  # **************************************
  #        Boxplot graph
  # **************************************
  # Render plot for data_processing_1
  output$bar_boxplot_1 <- renderPlot({
    data <- data_processing_1()
    if (is.null(data)) {
      return(NULL)
    }
    plot_data <- data[[2]]  # Extract the relevant part of the data
    plot <- generate_plot(plot_data, "Boxplot of Numeric Variables (Model 1)")
    print(plot)
    vals$bar_boxplot_1 <- plot
  }, bg = "#003D39")
  # Render plot for data_processing_2
  output$bar_boxplot_2 <- renderPlot({
    data <- data_processing_2()
    if (is.null(data)) {
      return(NULL)
    }
    plot_data <- data[[2]]  # Extract the relevant part of the data
    plot <- generate_plot(plot_data, "Boxplot of Numeric Variables (Model 2)")
    print(plot)
    vals$bar_boxplot_2 <- plot
  }, bg = "#003D39")
  # **************************************
  #        Heatmap graph
  # **************************************
  # Setting data
  is_heatmap_available <- function(data) {
    if (is.null(data)) {
      return(FALSE)
    }
    
    numeric_data <- data %>%
      select_if(is.numeric) %>%
      select(!matches("Ads|Id|plot")) %>%
      drop_na()
    
    return(nrow(numeric_data) > 1)  # Heatmap is available if there are multiple numeric rows
  }
  # Generate heatmap
  output$heatmapAvailable_1 <- reactive({
    data <- data_processing_1()
    data <- data[[2]]
    is_heatmap_available(data)
  })
  output$heatmapAvailable_2 <- reactive({
    data <- data_processing_2()
    data <- data[[2]]
    is_heatmap_available(data)
  })
  # Render heatmap
  output$heatmap_1 <- renderPlot({
    data <- data_processing_1()
    data <- data[[1]]
    heatmap <- generate_heatmap(data, "Heatmap of Pearson correlation coefficient matrix (Model 1)")
    vals$heatmap_1 <- heatmap  # Store the heatmap in vals
    suppressMessages(print(heatmap))
  }, bg = "white")
  output$heatmap_2 <- renderPlot({
    data <- data_processing_2()
    data <- data[[2]]
    heatmap <- generate_heatmap(data, "Heatmap of Pearson correlation coefficient matrix (Model 2)")
    vals$heatmap_2 <- heatmap  # Store the heatmap in vals
    suppressMessages(print(heatmap))
  }, bg = "white")
  # check heatmaps on backend
  outputOptions(output, "heatmapAvailable_1", suspendWhenHidden = FALSE)
  outputOptions(output, "heatmapAvailable_2", suspendWhenHidden = FALSE)
  ## **************************************
  ##        Pie-Chart 
  ## **************************************
  # Setting data
  is_pie_chart_available <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
      return(FALSE)
    }
    
    categorical_data <- data %>%
      mutate_if(is.character, as.factor) %>%
      select_if(is.factor) %>%
      select(!matches("Ads|Id|plot"))
    
    return(ncol(categorical_data) > 0)  # Pie chart is available if there are categorical columns
  }
  # generate pie-chart
  output$piechartAvailable_1 <- reactive({
    data <- data_processing_1()
    data <- data[[2]]
    is_pie_chart_available(data)
  })
  # Render pie-chart
  output$pie_charts_1 <- renderPlot({
    data <- data_processing_1()
    data <- data[[2]]
    pie_charts <- generate_pie_charts(data)
    vals$pie_charts_1 <- pie_charts  # Store the pie charts in vals
    print(pie_charts)
  }, bg = "white")
  # check piechart on backend
  outputOptions(output, "piechartAvailable_1", suspendWhenHidden = FALSE)
  ## **************************************
  ##        Downloaded file function
  ## **************************************
  output$downloadData_Miti_ForMan <- downloadHandler( 
    filename = function() {"OutputData_CSF_Mitigation_ForManInt.xlsx"},
    content = function(file) {
      data <- data_processing_1()
      data <- data[[2]]
      if (!is.null(data)) {
        write.xlsx(data, file)
      }
    })
  output$downloadData_Miti_Plot <- downloadHandler( 
    filename = function() {"OutputData_CSF_Mitigation_Plot.xlsx"},
    content = function(file) {
      data <- data_processing_1()
      data <- data[[1]]
      if (!is.null(data)) {
        write.xlsx(data, file)
      }
    })
  #
  output$downloadData_Adap_ForMan <- downloadHandler(
    filename = function() {"OutputData_Adaptation_ForManInt.xlsx"},
    content = function(file) {
      data <- data_processing_2()
      data <- data[[2]]
      if (!is.null(data)) {
        write.xlsx(data, file)
      }
    })
  #
  output$downloadData_Adap_Plot <- downloadHandler(
    filename = function() {"OutputData_Adaptation_Plot.xlsx"},
    content = function(file) {
      data <- data_processing_2()
      data <- data[[1]]
      if (!is.null(data)) {
        write.xlsx(data, file)
      }
    })
  # **************************************
  #        Download Report data (.pdf)
  # **************************************
  vals <- reactiveValues(
    data_summary_1 = NULL,
    heatmap_1 = NULL,
    pie_charts_1 = NULL,
    bar_boxplot_1 = NULL,
    map1 = NULL,
    
    data_summary_2 = NULL,
    heatmap_2 = NULL,
    bar_boxplot_2 = NULL,
    map2 = NULL
  )
  #
  output$downloadReportPDF_1 <- downloadHandler(
    filename = function() { "Report_CSF_Mitigation.pdf" },
    content = function(file) {
      generate_report(
        file = file,
        type = "mitigation",
        data_processing_function = data_processing_1,
        vals = vals
      )
    }
  )
  # **************************************
  output$downloadReportPDF_2 <- downloadHandler(
    filename = function() { "Report_CSF_Adaptation.pdf" },
    content = function(file) {
      generate_report(
        file = file,
        type = "adaptation",
        data_processing_function = data_processing_2,
        vals = vals
      )
    }
  )
  # **************************************
  #        Selection Input data option
  # **************************************
  observe({
    req(input$file)
    data <- openxlsx::read.xlsx(input$file$datapath)
    # **************************************************************************
    # Mitigation pillar
    # **************************************************************************
    updateSelectInput(session, "I1_plot_col", choices = names(data))
    updateSelectInput(session, "I1_height_col", choices = names(data))
    updateSelectInput(session, "I1_dbh_col", choices = names(data))
    updateSelectInput(session, "I1_specie_col", choices = names(data))
    updateSelectInput(session, "I1_ForManInt_col", choices = names(data))
    # 
    updateSelectInput(session, "I2_plot_col", choices = names(data))
    updateSelectInput(session, "I2_dom_col", choices = names(data))
    updateSelectInput(session, "I2_vol_col", choices = names(data))
    updateSelectInput(session, "I2_ForManInt_col", choices = names(data))
    #
    updateSelectInput(session, "I3_plot_size_col", choices = names(data))
    updateSelectInput(session, "I3_plot_col", choices = names(data))
    updateSelectInput(session, "I3_TH_tot_col", choices = names(data))
    updateSelectInput(session, "I3_DBH_col", choices = names(data))
    updateSelectInput(session, "I3_L_tot_col", choices = names(data))
    updateSelectInput(session, "I3_Dhalf_col", choices = names(data))
    updateSelectInput(session, "I3_ForManInt_col", choices = names(data))
    # 
    updateSelectInput(session, "I4_plot_size_col", choices = names(data))
    updateSelectInput(session, "I4_plot_col", choices = names(data))
    updateSelectInput(session, "I4_TH_tot_col", choices = names(data))
    updateSelectInput(session, "I4_DBH_col", choices = names(data))
    updateSelectInput(session, "I4_L_tot_col", choices = names(data))
    updateSelectInput(session, "I4_Dhalf_col", choices = names(data))
    updateSelectInput(session, "I4_ForManInt_col", choices = names(data))
    # 
    updateSelectInput(session, "I5_ForManInt_col", choices = names(data))
    updateSelectInput(session, "I5_plot_col", choices = names(data))
    updateSelectInput(session, "I5_L_col", choices = names(data))
    updateSelectInput(session, "I5_Dmax_col", choices = names(data))
    updateSelectInput(session, "I5_Dmin_col", choices = names(data))
    # **************************************************************************
    # Adaptation pillar
    # **************************************************************************
    updateSelectInput(session, "I51_plot_col", choices = names(data))
    updateSelectInput(session, "I51_height_col", choices = names(data))
    updateSelectInput(session, "I51_dbh_col", choices = names(data))
    updateSelectInput(session, "I51_specie_col", choices = names(data))
    updateSelectInput(session, "I51_ForManInt_col", choices = names(data))
    
    
  })
  # **************************************
  #        Reset
  # ************************************** 
  observeEvent(input$reset, {
    session$reload()
  })
}
# **************************************************************************
# End
# **************************************************************************