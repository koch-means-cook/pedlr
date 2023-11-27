library(here)

# Source own render wrapper (Rmarkdown)
source(file.path(here::here(),
                 'code',
                 'utils',
                 'Render_to_derivatives.R',
                 fsep = .Platform$file.sep))

book_path = file.path(here::here(),
                      'code',
                      'analysis',
                      'results_01.Rmd',
                      fsep = .Platform$file.sep)
out_dir = file.path(here::here(),
                    'derivatives',
                    'analysis',
                    fsep = .Platform$file.sep)

# Render Notebook in derivatives folder
Render_to_derivatives(book_path = book_path,
                      out_dir = out_dir)
