library(here)

# Source own render wrapper (Rmarkdown)
source(file.path(here::here(),
                 'code',
                 'utils',
                 'Render_to_derivatives.R',
                 fsep = .Platform$file.sep))

# Unlock data set to allow to make changes (see datalad)
derivatives_dir = file.path(here::here(),
                            'derivatives',
                            fsep = .Platform$file.sep)
system(paste0("datalad unlock ", derivatives_dir),
       intern = TRUE)

book_path = file.path(here::here(),
                      'code',
                      'simulation',
                      'simulation_analysis.qmd',
                      fsep = .Platform$file.sep)
out_dir = file.path(here::here(),
                    'derivatives',
                    'simulation',
                    fsep = .Platform$file.sep)

# Render Notebook in derivatives folder
Render_to_derivatives(book_path = book_path,
                      out_dir = out_dir)
