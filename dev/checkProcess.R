# Setup

library(devtools)
library(roxygen2)

# Vignette building

devtools::install(build_vignettes = TRUE)

# Spell checking

spell_check()

rhub::validate_email(email = "yuehu@tsinghua.edu.cn", token = "345011f4ca60404abf76007e9ae89e3e")

check_rhub(email = "yuehu@tsinghua.edu.cn")
