# ******************************************************************************
# This is CSF-Ind web-based app 
# ******************************************************************************
# Usa source() per caricare il codice base.R dal tuo repository GitHub
source("https://raw.githubusercontent.com/Cesarito2021/OwnLibraries/main/base.R")

# Ora puoi utilizzare la variabile 'ui' e 'server' definiti in base.R
shinyApp(ui, server, options = list(height = 1300))

# Run the application 
shinyApp(ui, server, options = list(height = 1300))
