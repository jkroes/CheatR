#!/usr/bin/env Rscript

# My cheatsheets are stored in ~/Sync/cheatsheets

# https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script
#' current script file (in full path)
#' @description current script file (in full path)
#' @examples
#' works with Rscript, source() or in RStudio Run selection, RStudio Console
#' @export
ez.csf <- function() {
  # http://stackoverflow.com/a/32016824/2292993
  cmdArgs = commandArgs(trailingOnly = FALSE)
  needle = "--file="
  match = grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript via command line
    return(normalizePath(sub(needle, "", cmdArgs[match])))
  } else {
    ls_vars = ls(sys.frames()[[1]])
    if ("fileName" %in% ls_vars) {
      # Source'd via RStudio
      return(normalizePath(sys.frames()[[1]]$fileName))
    } else {
      if (!is.null(sys.frames()[[1]]$ofile)) {
        # Source'd via R console
        return(normalizePath(sys.frames()[[1]]$ofile))
      } else {
        # RStudio Run Selection
        # http://stackoverflow.com/a/35842176/2292993
        pth = rstudioapi::getActiveDocumentContext()$path
        if (pth!='') {
          return(normalizePath(pth))
        } else {
          # RStudio Console
          tryCatch({
            pth = rstudioapi::getSourceEditorContext()$path
            pth = normalizePath(pth)
          }, error = function(e) {
            # normalizePath('') issues warning/error
            pth = ''
          }
          )
          return(pth)
        }
      }
    }
  }
}

option_list = list(
  optparse::make_option(c("-t", "--title"),
                        type = "character",
                        default = NULL,
                        help = "Cheatsheet title."),
  optparse::make_option(c("-i", "--input"),
                        type = "character",
                        default = NULL,
                        help = "Input spreadsheet path. Relative paths to the working directory may be specified."),
  optparse::make_option(c("-s", "--sheet"),
                        type = "character",
                        default = "Cheat",
                        help = "Sheet name"),
  optparse::make_option(c("-o", "--output"),
                        type = "character",
                        default = NULL,
                        help = "Output cheatsheet name or path (relative or absolute). If NULL, the output file will be named after the input file. If NULL or the file extension is missing, an extension will be added based on the output format.")
)

opt_parser <- optparse::OptionParser(option_list = option_list)
opt <- optparse::parse_args(opt_parser)

if (is.null(opt$title)) {
  optparse::print_help(opt_parser)
  stop("Title is missing.")
}

if (is.null(opt$input)) {
  optparse::print_help(opt_parser)
  stop("Input is missing.")
}

# Normalized path required for file.access
input <- normalizePath(opt$input)
if (!(file.access(input, 4) + 1)) {
  optparse::print_help(opt_parser)
  stop("Input is not readable. Does it exist?")
}

# NULL output must be handled, or render will name it like the input .Rmd file
# (i.e., ConstructCheat.Rmd) rather than the input parameter passed to .Rmd file
if (!is.null(opt$output)) {
  output <- basename(opt$output)
  outdir <- dirname(opt$output)
} else {
  output <- tools::file_path_sans_ext(input)
  outdir <- dirname(input)
}
  
rmarkdown::render(input = file.path(dirname(ez.csf()), "ConstructCheat.Rmd"),
                  output_file = output,
                  output_dir = outdir,
                  params = list(file = input, sheet = opt$sheet, title = opt$title))

# Remove the intermediate markdown file created by ConstructCheat.Rmd
file.remove(file.path(dirname(ez.csf()), "__temp__.html"))