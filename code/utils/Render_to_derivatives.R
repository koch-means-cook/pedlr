library(here)
library(rmarkdown)

Render_to_derivatives = function(book_path,
                                 out_dir){
  
  # book_path = file.path(here::here(), 'code', 'analysis', 'demographic.Rmd', fsep = .Platform$file.sep)
  # out_dir = file.path(here::here(), 'derivatives', 'analysis', fsep = .Platform$file.sep)
  
  # Create out_dir if it does not exist yet
  if(!dir.exists(out_dir)){
    dir.create(out_dir)
  }
  
  # Render notebook with options
  rmarkdown::render(input = book_path,
                    output_dir = out_dir,
                    output_format = rmarkdown::html_document(self_contain = TRUE, 
                                                             toc_float = TRUE,
                                                             code_folding = 'hide',
                                                             toc = TRUE,
                                                             toc_depth = 3,
                                                             math_method = 'katex'))
  
}