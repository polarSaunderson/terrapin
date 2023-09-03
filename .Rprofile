source("renv/activate.R")

# Add any rstudioapi calls to run upon opening the project in RStudio
setHook("rstudio.sessionInit",
        function(newSession) {
          if (newSession) {
            rstudioapi::navigateToFile("DESCRIPTION")
            rstudioapi::navigateToFile("README.md")
            message("< Hi devDomi, let's get to work on some terrapin code!")
          }
        },
        action = "append")
