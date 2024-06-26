# Project:   plotmihd
# Objective: Deployment script
# Author:    Edoardo Costantini
# Created:   2023-04-26
# Modified:  2023-04-26
# Notes:     The application needs an entry point which should be named app.R 
#            and be situated in the root of the package, i.e. where DESCRIPTION 
#            and NAMESPACE are located.

# Load package
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

# Run shiny app
plotmihd::start_app()