# Install & library
if (!require('devtools')) install.packages('devtools')
devtools::install_github('ericmkeen/catRlog')
library(catRlog)

# Set working directory to your project folder
# (using a shortcut from the rstudioAPI package):
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Setup folders within your directory
setup_project()

# Explore digital catalog
catalog()

# Worklow
score()
match()
review_matches()
register_new_ids()
update_catalog()
update_events()
print_catalog()
analysis_dashboard()

