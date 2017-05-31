#' Read biblographic data from Web of Science.
#'
#' This function allows you to read WIN UTF 8, TSV files from the Web of Science.
#' @param path_to_file The full path of the file you want to open
#' @keywords Web of Science
#' @export
#' @examples
#' read_from_wos(file.choose())
read_from_wos <- function(path_to_file) {
  my_data <- read.table(path_to_file
                        , header=T
                        , sep="\t"
                        , fill= T
                        , quote =""
                        , row.names = NULL
                        , stringsAsFactors = F
                        , check.names=F
                       )
  colnames(my_data) <- c("PT", colnames(my_data)[3:length(colnames(my_data))], "END")
  my_data["END"] <- NULL
  return(my_data)
}


#' Read patent report data from Thomson Innovation.
#'
#' This function allows you to read .CSV reports from Thomson Innovation.
#' @param path_to_file The full path of the file you want to open
#' @param skip Defaults to 1. The number of rows to skip before starting readind the data
#' @keywords Thomson Innovation
#' @seealso \code{\link{read_from_ti_fast}}
#' @export
#' @examples
#' read_from_ti(file.choose())
read_from_ti <- function(path_to_file, skip = 1) {
  my_data <- read.csv(path_to_file
                      , skip = skip
                      , header = TRUE
                      , check.names = FALSE
                      , stringsAsFactors = FALSE
                     )
  
  #Column displacement correction
  first_column <- row.names(my_data)
  data2 <- cbind(first_column, my_data)
  data2$first_column <- as.character(data2$first_column)
  names(data2) <- c(names(my_data), "final_column" )
  my_data <- unique(data2[,-56])
  rm(data2)
  return(my_data)
}


#' Fast reading of patent report data from Thomson Innovation.
#'
#' This function allows you to read .CSV reports from Thomson Innovation.
#' 
#' Encoding problem raises when Thomson Innovation interface is in Japanese, using English interface is advised.
#' @param path_to_file The full path of the file you want to open
#' @param skip Defaults to 1. The number of rows to skip before starting readind the data
#' @keywords Thomson Innovation
#' @seealso \code{\link{read_from_ti}}
#' @export
#' @examples
#' read_from_ti_fast(file.choose())
read_from_ti_fast <- function(path_to_file, skip = 1) {
  my_data <- fread(path_to_file
                   , fill = TRUE
                   , select = 1:55
                   , skip = 1
                   , header=TRUE
                   , check.names = FALSE
                   , stringsAsFactors = FALSE)
  return(my_data)
}


#' Read files generated in Fukan System.
#'
#' This function allows you to read report analysis from Fukan System
#' @param path_to_file The full path of the file you want to open
#' @keywords Fukan System
#' @seealso \code{\link{write_for_fukan}}
#' @export
#' @examples
#' read_from_fukan(file.choose())
read_from_fukan <- function(path_to_file) {
  my_data <- fread(path_to_file
                   , header = TRUE
                   , stringsAsFactors = FALSE
                   , quote = ""
                  )
  return(my_data)
}

#' Read .CSV data frames.
#'
#' This function allows you to read .CSV files
#' 
#' Files are expected to have header. Contents are forced to strings.
#' @param path_to_file The full path of the file you want to open
#' @seealso \code{\link{read_from_others_tsv}}
#' @export
#' @examples
#' read_from_others_csv(file.choose())
read_from_others_csv <- function(path_to_file) {
  my_data <- fread(path_to_file
                   , header = TRUE
                   , stringsAsFactors = FALSE
                   , quote = ''
                  )
  return(my_data)
}



#' Read .TSV data frames.
#'
#' This function allows you to read .TSV files.
#' 
#' Files are expected to have header. Contents are forced to strings.
#' @param path_to_file The full path of the file you want to open
#' @seealso \code{\link{read_from_others_csv}}
#' @export
#' @examples
#' read_from_others_tsv(file.choose())
read_from_others_tsv <- function(path_to_file) {
  my_data <- read.csv(path_to_file
                      , header = TRUE
                      , sep = "\t"
                      , check.names = FALSE
                      , stringsAsFactors = FALSE
                     )
  return(my_data)
}


#' Writes data sets to be used in Fukan System
#'
#' This function writes files that can be used again in Fukan system to create network analysis.
#' 
#' @param data_file The data frame to use
#' @param file_name The name of the file in the format "*.tsv"
#' @keywords Fukan System
#' @seealso \code{\link{read_from_fukan}}
#' @export
#' @examples
#' write_for_fukan(my_data, 'my_data.tsv')
write_for_fukan <- function(data_file, file_name) {
  write.table(data_file
              , file = file_name
              , row.names = FALSE
              , sep = "\t"
              ,  quote = FALSE
             )
}

