# ******************************************************************************
# This is CSF-Ind web-based app 
# ******************************************************************************
#  repository GitHub
source("https://raw.githubusercontent.com/Cesarito2021/OwnLibraries/main/base.R")


# Run the application 
shinyApp(ui, server, options = list(height = 1300))
