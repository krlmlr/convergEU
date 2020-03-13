#' Create a country fiche for an indicator
#'
#' An auxiliary function to compile a rmarkdown file to produce a country fiche in
#' html format within the output directory.
#'
#' Note that most of function arguments are passed as strings of characters
#' instead of object names. For example, if the object of a dataset
#' in the workspace is myTB, the parameter is set like  workDF='myTB'
#' instead of workDF=myTB as one may expect.
#' Furthermore, the dataset must be complete, that is without missing values.
#' Note also that connection to Internet should be available when invoking the
#' function to properly rendering the results in the html file
#'
#'
#' @param workDF name (string) of the dataset  with all countries
#'                 contributing to average
#' @param countryRef  country of main interest
#' @param otherCountries  other countries for comparison
#' @param time_0 starting time
#' @param time_t ending time
#' @param tName name of the variable containing times (years)
#' @param indiType type of indicator "lowBest" or "highBest"
#' @param aggregation label indicator the reference group of countries ('custom'
#'                    if not pre-coded)
#' @param x_angle axis orientation for time labels
#' @param dataNow date of production of this country fiche
#' @param author  author of this report
#' @param outFile name of the output file (without path)
#' @param outDir  output directory, eventualy not existing (only one level allowed)
#' @param indiName  name of the considered indicator
#'
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#' @examples
#'
#' \donttest{
#' go_ms_fi(
#'      workDF ='myTB',
#'      countryRef ='DE',
#'      otherCountries = "c('IT','UK','FR','BE','AT')",
#'      time_0 = 2003,
#'      time_t = 2014,
#'      tName = 'time',
#'      indiType = "highBest",
#'      aggregation= 'EU27',
#'      x_angle=  45,
#'      dataNow=  Sys.time(),
#'      author = 'A.Student',
#'      outFile = 'Counfic-DE-2015',
#'      outDir = 'F:/STORE/tt-fiche',
#'      indiName= "emp_20_64_MS"
#'      )
#'
#' go_ms_fi(
#'      workDF ='myTB',
#'      countryRef ='IT',
#'      otherCountries = 'c("DE","UK")',
#'      time_0 = 2005,
#'      time_t = 2015,
#'      tName = 'time',
#'      aggregation= 'EU27',
#'      x_angle=  45,
#'      dataNow=  Sys.time(),
#'      author = 'A.Student',
#'      outFile = 'Counfic-IT-2015',
#'      outDir = "/tt-fish",
#'      indiName= "emp_20_64_MS"
#'      )
#'
#' go_ms_fi(
#'      workDF ='myTB',
#'      countryRef ='DE',
#'      otherCountries = "c('IT','UK','FR','BE','AT')",
#'      time_0 = 2003,
#'      time_t = 2014,
#'      tName = 'time',
#'      indiType =  "highBest",
#'      aggregation= 'EU12',
#'      x_angle=  45,
#'      dataNow=  Sys.time(),
#'      author = 'A.Student',
#'      outFile = 'Counfic-DE-2015',
#'      outDir = '/tt-fish',
#'      indiName= "emp_20_64_MS"
#'      )
#'
#' # artificially created dataset
#' myTTB <- emp_20_64_MS
#' names(myTTB)<- c("time",paste("PP",1:28,sep="-"))
#' go_ms_fi(
#'        workDF ='myTTB',
#'        indiName= "lifesatisf",
#'        countryRef ='PP-21',
#'        otherCountries = "c('PP-1','PP-24','PP-11','PP-28','PP-13')",
#'        time_0 = 2005,
#'        time_t = 2010,
#'        tName = 'time',
#'        indiType =  "highBest",
#'        aggregation= 'custom',
#'        x_angle=  45,
#'        dataNow=  Sys.time(),
#'        author = 'A.Student',
#'        outFile = 'country-test-custom',
#'        outDir = '/tt-fish'
#'      )
#'}
#'
#' @export
go_ms_fi <-  function(
  workDF=NA,
  countryRef= NA,
  otherCountries= c(NA,NA),
  time_0= NA,
  time_t= NA,
  tName = NA,
  indiType= NA,
  aggregation= NA,
  x_angle=  NA,
  dataNow=  NA,
  author= NA,
  outFile= NA,
  outDir = NA,
  indiName= NA
){
   # forcing otherCountries to be  specified
  if(any(is.na(otherCountries))){
    obj_out <- convergEU_glb()$tmpl_out
    obj_out$err <- paste("Error: one or more missing countries in otherCountries object. ")
    return(obj_out)
  }
  # check if missing values are present
  curTB <- get(workDF)
  if( any(!stats::complete.cases(curTB))){
    obj_out <- convergEU_glb()$tmpl_out
    obj_out$err <- paste("Error: one or more missing values (NAs) in the dataframe. ",
                         "Please perform imputation of missing values (NA) before making fiches.")
    return(obj_out)
  }
  #
  sourceFilecss <- system.file("extdata", "EUF.css", package = "convergEU")
  sourceFile1 <- system.file("extdata", "country_fi_2.Rmd", package = "convergEU")
  sourceFile2 <- system.file("extdata", "eurofound.jpg", package = "convergEU")
  if( is.na(outFile)) {
    outFile2 <- paste0("country-fiche-", countryRef, "-", time_0,"-",time_t, ".html")
    outFile <- paste0("country-fiche-", countryRef, "-", time_0,"-",time_t)
  }else{
    outFile2 <- paste0(outFile,".html")
  }
  if( is.na(outDir)) {
    outDir <- file.path(getwd(),"out_dir_counvergEU")
  }
  # check
  resDE <- dir.exists(outDir)
  if (!resDE) {
    dir.create(outDir, FALSE)
  }
  # full path
  outFcss <- file.path(outDir,"EUF.css")
  outPF <- file.path(outDir,outFile2)
  sourcePF1 <- file.path(outDir,"country_fi_2.Rmd")
  sourcePF2 <- file.path(outDir,"eurofound.jpg")
  # copy them
    file.copy(from = sourceFile1,
              to = sourcePF1,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = sourceFile2,
              to = sourcePF2,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = sourceFilecss,
             to = outFcss,
             overwrite = TRUE,
             recursive = FALSE,
             copy.mode = TRUE, copy.date = FALSE);

  # copy template
  rmarkdown::render(input=sourcePF1,
                    output_format="html_document",
                    params = list(
                      workingDF=workDF,
                      time_0= time_0,
                      time_t= time_t,
                      timeName= tName,
                      country= countryRef,
                      otherCountries= otherCountries,
                      indiType=indiType,
                      indiName=indiName,
                      aggregation= aggregation,
                      x_angle=  45,
                      dataNow=  dataNow,
                      auth= author,
                      outFile = outFile,
                      outDir =  outDir
                    ),
                    output_file = outPF)
}
