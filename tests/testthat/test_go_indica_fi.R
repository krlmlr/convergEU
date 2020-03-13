
context("Indicator fiche")



library(convergEU)
require(tibble)
require(devtools)


#  undebug(go_indica_fi)


test_that("Basic operation", {


#  debug(go_indica_fi)

testest <- function(){

  system('rm /media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish/indica_fi_2.Rmd')

  go_indica_fi(
    time_0 = 2004,
    time_t = 2018,
    timeName = 'time',
    workDF = 'myTB' ,
    indicaT = 'emp_20_64_MS',
    indiType = c('highBest','lowBest')[1],
    seleMeasure = 'all',
    seleAggre = 'EU28',
    x_angle =  45,
    data_res_download =  FALSE,
    auth = 'A.Student',
    dataNow =  '2019/01/31',
    outFile = "test_indica-fi-emp_20_64_MS",
    outDir = "/media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish"
    )

  browseURL('/media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish/test_indica-fi-emp_20_64_MS.html')





  system('rm /media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish/indica_fi_2.Rmd')
   folder_tmp <- "/media/fred/STOREg7/REPO_GIT/TENDER-EUF/tt-fish"
   folder_tmp <-"/media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish"

  go_indica_fi(
    time_0 = 2004,
    time_t = 2014,
    timeName = 'time',
    workDF = 'emp_20_64_MS' ,
    indicaT = 'emp_20_64',
    indiType = c('highBest','lowBest')[1],
    seleMeasure = 'all',
    seleAggre = 'EU28',
    x_angle =  45,
    data_res_download =  FALSE,
    auth = 'A.Student',
    dataNow =  '2019/01/31',
    outFile = "test_indica-fi-emp_20_64_MS",
    outDir = folder_tmp
  )

  browseURL(file.path(folder_tmp,'test_indica-fi-emp_20_64_MS.html'))


  system('rm /media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish/indica_fi_2.Rmd')
  folder_tmp <- "/media/fred/STOREg7/REPO_GIT/TENDER-EUF/tt-fish"
  folder_tmp <-"/media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish"

  go_indica_fi(
    time_0 = 2002,
    time_t = 2010,
    timeName = 'time',
    workDF = 'emp_20_64_MS' ,
    indicaT = 'emp_20_64',
    # indiType = 'highBest',
    indiType = 'lowBest',
    seleMeasure = 'all',
    seleAggre = 'EU28',
    x_angle =  45,
    data_res_download =  FALSE,
    auth = 'A.Student',
    dataNow =  '2019/10/16',
    outFile = "newtest_IT-emp_20_64_MS",
    outDir = folder_tmp
    )

    browseURL(file.path(folder_tmp,'newtest_IT-emp_20_64_MS.html'))











  ## ISSUE da Chiara Litardi  25 Giugno 2019

system('rm /media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish/indica_fi_2.Rmd')

go_indica_fi(
    time_0 = 2002,
    time_t = 2010,
    timeName = 'time',
    workDF = 'emp_20_64_MS' ,
    indicaT = 'emp_20_64',
    # indiType = 'highBest',
    indiType = 'lowBest',
    seleMeasure = 'all',
    seleAggre = 'EurozoneBITUR',
    x_angle =  45,
    data_res_download =  FALSE,
    auth = 'A.Student',
    dataNow =  '2019/05/16',
    outFile = "newtest_IT-emp_20_64_MS",
    outDir = "/media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish"
  )

browseURL('/media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish/newtest_IT-emp_20_64_MS.html')



  system('rm /media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish/indica_fi_2.Rmd')

  go_indica_fi(
    time_0 = 2002,
    time_t = 2010,
    timeName = 'time',
    workDF = 'emp_20_64_MS' ,
    indicaT = 'emp_20_64',
    indiType = 'highBest',
    #indiType = 'lowBest',
    #    seleMeasure = c(),
    #seleMeasure = c('beta','sigma'),
    #seleMeasure = c('delta','sigma','beta','gamma'),
    #    seleMeasure = c("delta",'gamma')
    #seleMeasure = c("beta","sigma"),
    seleMeasure = c('all'),
    seleAggre = 'EU28',#Eurozone',
    x_angle =  45,
    data_res_download =  FALSE,
    auth = 'A.Student',
    dataNow =  '2019/05/16',
    outFile = "newtest_IT",
    outDir = "/media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish"
  )

  browseURL('/media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish/newtest_IT.html')




  # custom aggergation
  system('rm /media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish/indica_fi_2.Rmd')
  myTTB <- emp_20_64_MS
  dim(myTTB)
  names(myTTB)<- c("time",paste("PP",1:28,sep="~"))

  go_indica_fi(
    time_0 = 2005,
    time_t = 2010,
    timeName = 'time',
    workDF = 'myTTB' ,
    indicaT = 'emp_20_64',
    indiType = 'highBest',
    #indiType = 'lowBest',
    #    seleMeasure = c(),
    #seleMeasure = c('beta','sigma'),
    #seleMeasure = c('delta','sigma','beta','gamma'),
    #    seleMeasure = c("delta",'gamma')
    #seleMeasure = c("beta","sigma"),
    seleMeasure = c('all'),
    seleAggre = 'custom',
    x_angle =  45,
    data_res_download =  FALSE,
    auth = 'A.Student',
    dataNow =  '2019/10/16',
    outFile = "newtest_IT",
    outDir = "/media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish"
  )

  browseURL('/media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish/newtest_IT.html')

  # custom aggergation
  system('rm /media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish/indica_fi_2.Rmd')
  myTTB <-   tibble::tribble(
      ~time, ~UK, ~DE, ~IT,
      2005,   10 , 7  , 6,
      2006,   10 , 7    ,  6, #
      2007,    10,  7 ,   6#
    )

  go_indica_fi(
    time_0 = 2005,
    time_t = 2007,
    timeName = 'time',
    workDF = 'myTTB' ,
    indicaT = 'testerIndica',
    indiType = 'highBest',
    #indiType = 'lowBest',
    #    seleMeasure = c(),
    #seleMeasure = c('beta','sigma'),
    #seleMeasure = c('delta','sigma','beta','gamma'),
    #    seleMeasure = c("delta",'gamma')
    #seleMeasure = c("beta","sigma"),
    seleMeasure = c('all'),
    seleAggre = 'custom',
    x_angle =  45,
    data_res_download =  FALSE,
    auth = 'A.Student',
    dataNow =  '2019/12/07',
    outFile = "indica_custom",
    outDir = "/media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish"
  )


  browseURL('/media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish/indica_custom.html')


  # strict converg highBest
  myTTB <-   tibble::tribble(
    ~time, ~UK, ~DE, ~IT,
    2005,   10 , 5  , 2,
    2006,   12 , 9  , 6, #
    2007,   10,  9  , 6#
  )
  # strict diverg highBest
  myTTB <-   tibble::tribble(
    ~time, ~UK, ~DE, ~IT,
    2005,   10 , 5  , 2,
    2006,   12 , 9  , 6, #
    2007,   25,  9  , 10#
  )


  go_indica_fi(
    time_0 = 2005,
    time_t = 2007,
    timeName = 'time',
    workDF = 'myTTB' ,
    indicaT = 'testerIndica',
    indiType = 'highBest',
    #indiType = 'lowBest',
    #    seleMeasure = c(),
    #seleMeasure = c('beta','sigma'),
    #seleMeasure = c('delta','sigma','beta','gamma'),
    #    seleMeasure = c("delta",'gamma')
    #seleMeasure = c("beta","sigma"),
    seleMeasure = c('all'),
    seleAggre = 'custom',
    x_angle =  45,
    data_res_download =  FALSE,
    auth = 'A.Student',
    dataNow =  '2019/12/07',
    outFile = "indica_custom",
    outDir = "/media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish"
  )

  browseURL('/media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish/indica_custom.html')



  # strict converg lowBest
  myTTB <-   tibble::tribble(
    ~time, ~UK, ~DE, ~IT,
    2005,   10 , 5  , 2,  # 3 7
    2006,   12 , 9  , 6, #
    2007,   7,  6  , 4  #  2, 3
  )
  # strict diverg lowBest
  myTTB <-   tibble::tribble(
    ~time, ~UK, ~DE, ~IT,
    2005,   10 , 4  , 2,
    2006,   12 , 5  , 6, #
    2007,   25,  4  , 10#
  )
  #   diverg lowBest
  myTTB <-   tibble::tribble(
    ~time, ~UK, ~DE, ~IT,
    2005,   10 , 14  , 8,
    2006,   12 , 10  , 7, #
    2007,   25,   7  , 6#
  )

  go_indica_fi(
    time_0 = 2005,
    time_t = 2007,
    timeName = 'time',
    workDF = 'myTTB' ,
    indicaT = 'testerIndica',
    indiType = 'lowBest',
    #indiType = 'lowBest',
    #    seleMeasure = c(),
    #seleMeasure = c('beta','sigma'),
    #seleMeasure = c('delta','sigma','beta','gamma'),
    #    seleMeasure = c("delta",'gamma')
    #seleMeasure = c("beta","sigma"),
    seleMeasure = c('all'),
    seleAggre = 'custom',
    x_angle =  45,
    data_res_download =  FALSE,
    auth = 'A.Student',
    dataNow =  '2019/12/07',
    outFile = "indica_custom",
    outDir = "/media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish"
  )

  browseURL('/media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish/indica_custom.html')





  system('rm /media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish/indica_fi_2.Rmd')

     negaTB <- emp_20_64_MS
     #negaTB[4,3]<- -5.18
     #negaTB[1,5]<- -15.18

     #negaTB[4,3]<- 0
     #negaTB[1,5]<- 0

    go_indica_fi(
      time_0 = 2002,
      time_t = 2010,
      timeName = 'time',
      workDF = 'negaTB' ,
      #workDF = 'emp_20_64_MS' ,
      indicaT = 'emp_20_64',
      # indiType = 'highBest',
      indiType = 'lowBest',
      #seleMeasure = c(),
      #seleMeasure = c('beta'),
      #seleMeasure = c('delta','sigma','beta',  'gamma'),
      #seleMeasure = c('gamma'),
      seleMeasure = c('all'),
      seleAggre = 'EA',
      x_angle =  45,
      data_res_download =  FALSE,
      auth = 'A.Student',
      dataNow =  '2019/05/16',
      outFile = "negafake",
      outDir = "/media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish"
    )

    browseURL('/media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish/negafake.html')


  TB <-  emp_20_64_MS
  TB[15,1]<- 2020
  go_indica_fi(time_0=2002,time_t=2020,
               timeName = 'time',
               workDF = "TB",
               indicaT = 'emp_20_64_MS',
               indiType = c("highBest","lowBest")[1],
               seleMeasure = 'all',
               seleAggre ='EU28',
               dataNow = Sys.time(),
               outFile = 'test_EA_indicator fiche',
               #outDir = 'C:/Users/cli/OneDrive - Eurofound/Policy Brief - Pillar of social rights',
               outDir = "/media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish",
               pdf_out = T)

  browseURL('/media/fred/STORE/PRJ/2018-TENDER-EU/STEP-1/bitbucketed/tt-fish/test_EA_indicator fiche.html')


} # incapsulator

  expect_equal(1, 1)
  #rm(TB)
})




