# setup.R - Automatically installs and loads required packages

# List of required packages
required_packages <- c(
  "ggplot2", "psych", "dplyr", "Rmisc", "car", "effectsize", 
  "ggsignif", "cowplot", "lmtest", "sandwich", "scoring", 
  "data.table", "tidyr", "afex", "lsr", "emmeans", "ggpubr", 
  "gridExtra", "Hmisc", "ltm", "psycho", "styler", "stringr", "usethis"
)

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) {
  message("Installing missing packages: ", paste(new_packages, collapse = ", "))
  install.packages(new_packages, dependencies = TRUE)
}

# Load all packages
lapply(required_packages, library, character.only = TRUE)

# Print message confirming successful loading
message("✔ All required packages are installed and loaded successfully!")

word_count <- function() {
  text <- readClipboard()  # Get highlighted text from clipboard
  text_combined <- paste(text, collapse = " ")  # Combine all lines/paragraphs into one string
  count <- str_count(text_combined, "\\S+")  # Count non-whitespace words
  print(paste("Total word count:", count))
}