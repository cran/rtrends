#' @title CRAN Stats for single package
#' @author Avi Blinder
#' @description Download CRAN data related to several packages in a range of dates
#' @param from_date  Starting date for downloading CRAN Repositories
#' @param to_date  end date for downloading CRAN Repositories
#' @param sel_package_name  Single package name for analysis
#' @param cran_log CRAN mirror name
#' @export

cran_stats_by_package <- function(from_date,to_date,sel_package_name,
                                  cran_log='http://cran-logs.rstudio.com/'){

  ##
  start_date <- lubridate::ymd(from_date)
  end_date <- lubridate::ymd(to_date)

  all_days <- seq(start_date, end_date, by = 'day')
  log_years <- lubridate::year(all_days)

  urls <- paste0(cran_log, log_years, '/', all_days, '.csv.gz')

  package_stats <- c()
  for (i in 1:length(urls)) {
    stats_df <- c()
    tmp <- tempfile()
    download.file(urls[i], tmp)
    unzipped_file <- read.csv( gzfile(tmp), sep="\t",header=TRUE,
                               stringsAsFactors=FALSE)
    rm(tmp)
    #
    names(unzipped_file) <- "ConcatCol"
    unzipped_file_df <- tidyr::separate(unzipped_file,'ConcatCol',sep = ",",
                                 into = c("date","time","size","r_version","r_arch","r_os",
                                          "package_name","package_version","country","ip_id"))
    unzipped_file_df_pck <- subset(unzipped_file_df,unzipped_file_df$package_name == sel_package_name)
    package_stats <- rbind(package_stats,unzipped_file_df_pck)
  }
  package_stats
}
######################################################################################
#' @return A dataframe with complete log data of a specific date
#' @title CRAN Stats for a specific date
#' @author Avi Blinder
#' @description Download a full CRAN Log for a specific date
#' @param Date date of CRAN log to be downloaded
#' @param cran_log CRAN mirror name
#' @export
#'

cran_stats_by_day <- function(Date,
                              cran_log='http://cran-logs.rstudio.com/'){

  log_date <- lubridate::ymd(Date)


  ##Create regex for package name
  urls <- paste0(cran_log,
                 lubridate::year(log_date), '/', log_date, '.csv.gz')

  package_stats <- c()
  stats_df <- c()
  tmp <- tempfile()
  download.file(urls, tmp)
  unzipped_file <- read.csv( gzfile(tmp), sep="\t",header=TRUE,
                             stringsAsFactors=FALSE)
  rm(tmp)
  #remove header
  names(unzipped_file) <- "ConcatCol"
  unzipped_file_df <- tidyr::separate(unzipped_file,'ConcatCol',sep = ",",
                               into = c("date","time","size","r_version","r_arch","r_os",
                                        "package_name","package_version","country","ip_id"))
}
#######################################################################################
#' @return dataframe with log information for selected packages
#' @title CRAN raw data for several packages in a range of dates
#' @author Avi Blinder
#' @description Download CRAN log data related to several packages in
#' a range of dates
#' @param from_date  Starting date for downloading CRAN Repositories
#' @param to_date  end date for downloading CRAN Repositories
#' @param sel_package_name  Single package name for analysis
#' @param cran_log CRAN mirror name
#' @export
#'
cran_stats_by_packages <- function(from_date,to_date,sel_package_name,
                                   cran_log='http://cran-logs.rstudio.com/'){


  ##
  start_date <- lubridate::ymd(from_date)
  end_date <- lubridate::ymd(to_date)

  all_days <- seq(start_date, end_date, by = 'day')
  log_years <- lubridate::year(all_days)

  urls <- paste0(cran_log, log_years, '/', all_days, '.csv.gz')

  package_stats <- c()
  for (i in 1:length(urls)) {
    stats_df <- c()
    tmp <- tempfile()
    download.file(urls[i], tmp)
    unzipped_file <- read.csv( gzfile(tmp), sep="\t",header=TRUE,
                               stringsAsFactors=FALSE)
    rm(tmp)
    #
    names(unzipped_file) <- "ConcatCol"
    unzipped_file_df <- tidyr::separate(unzipped_file,'ConcatCol',sep = ",",
                                 into = c("date","time","size","r_version","r_arch","r_os",
                                          "package_name","package_version","country","ip_id"))
    unzipped_file_df_pck <- subset(unzipped_file_df,unzipped_file_df$package_name %in% sel_package_name)
    package_stats <- rbind(package_stats,unzipped_file_df_pck)
  }
  package_stats
}

#' @title Merge stats with County names
#' @author Avi Blinder
#' @description Merge CRAN log data with County names
#' @param cran_log  dataftame containing CRAN log data
#' @param countries Countries lookup table
#' data(sample_log)
#' data(countries)
#' cran_summary(sample_log,countries)
#' @export

cran_summary <- function(cran_log,countries){

  multiple_pack_Stats_full <- merge(cran_log,countries,by.x = "country",by.y="country_iso_code")

}
