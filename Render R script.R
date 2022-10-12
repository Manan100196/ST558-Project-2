rmarkdown::render(input = "ST558_Project2_Group_2H.Rmd", 
                  output_file = "README.md",
                  output_options = list(
                    toc = TRUE,
                    toc_depth = 2,
                    toc_float = TRUE
                  ))
