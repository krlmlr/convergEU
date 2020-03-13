library(convergEU)
require(tibble)
require(devtools)



##########################################################################
context("Check calculations of convergence: beta.")

# debug(beta_conv)

test_that("Beta convergence,  scrambled time and  different name, two times.", {
  # soDat <- matrix(
  #   c(88,   1201, 868, 578,
  #     89,   1150, 978, 682,
  #     90,   998,  1250, 332,
  #     91,  1600,  1350, 802),ncol=4,byrow=T
  # )
  # scrambled data
  myTB2  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    90,   998,  1250, 332,
    88,   1201, 868, 578,
    89,   1150, 978, 682,
    91,  1600,  1350, 802
  )
  soDat <- myTB2[c(2,3,1,4),] # sorted
  # beta conv
  # debug(beta_conv)

  #res_sigori <- beta_conv(soDat,88,90,FALSE,timeName="years")
  #res_sig <- beta_conv(myTB2,88,90, FALSE,timeName="years")

  # order of times does not matter
  #expect_equal(res_sig$res$summary$coefficients[2,1],
  #       res_sigori$res$summary$coefficients[2,1]
  #       )

  res_sigori <- beta_conv(soDat,89,90,FALSE,timeName="years")
  #res_sig <- beta_conv(myTB2,89,90, FALSE,timeName="years")
  yUK <- diff(log(unlist(soDat[,2])))
  yDE <- diff(log(unlist(soDat[,3])))
  yIT <- diff(log(unlist(soDat[,4])))
  risposte <- c(yUK[2],yDE[2],yIT[2])
  xval <- log(c(1150,   978,   682))
  beta1 <- as.numeric(lm( risposte ~ xval)$coeff[2])
  expect_equal(as.numeric(res_sigori$res$model$coefficients[2]),
               beta1)
  #expect_equal(res_sigori$res$model$coefficients[2],
  #             beta1)

})



test_that("Beta convergence,  several  times.", {
  # scrambled data
  myTB2  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    90,   998,  1250, 332,
    88,   1201, 868, 578,
    89,   1150, 978, 682,
    91,  1600,  1350, 802
  )
  soDat <- myTB2[c(3,1,4),] # sorted senza prima riga
  soDat2 <- myTB2[c(2,3,1,4),] # sorted  e  prima riga
  # beta conv
  # debug(beta_conv)
  res_sigori <- beta_conv(soDat2,89,91,TRUE,timeName="years",useTau=FALSE)
  res_sig <- beta_conv(soDat,89,91, TRUE,timeName="years",useTau=FALSE)
  #
  expect_equal(as.numeric(res_sigori$res$model$coefficients[2]),
               as.numeric(res_sig$res$model$coefficients[2])
  )
  #calculations by hand
  soDat[,2:4] <- log( soDat[,2:4])
  risposte <- as.numeric(c(unlist(soDat[2,2:4]),unlist(soDat[3,2:4])
     #6.91,  7.13,   5.81, 7.38,  7.21, 6.69
    ) - c(unlist(soDat[1,2:4]),unlist(soDat[1,2:4])
      #7.05,  6.89 , 6.53, 7.05,  6.89, 6.53
    ))
  xval <-   as.numeric(c(
    #7.05,  6.89 , 6.53,7.05,  6.89 , 6.53
    unlist(soDat[1,2:4]), unlist(soDat[1,2:4])));
  beta1 <- as.numeric(lm( risposte ~ xval)$coeff[2])
  expect_equal(as.numeric(res_sigori$res$model$coefficients[2]),
               beta1)
  expect_equal(as.numeric(res_sig$res$model$coefficients[2]),
               beta1)

})



test_that("Beta convergence, failures.", {
  # scrambled data
  myTB2  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    90,   998,  1250, 332,
    88,   1201, 868, 578,
    89,   1150, 978, 682,
    91,  1600,  1350, 802
  )[c(2,3,1,4),]
  # beta conv
  # debug(beta_conv)
  res_sig <- beta_conv(myTB2,89,90, TRUE, timeName="pippo")
  expect_null(res_sig$res)

  res_sig <- beta_conv(myTB2,80,90, TRUE, timeName="years")
  expect_null(res_sig$res)

  res_sig <- beta_conv(myTB2,89,2000, TRUE, timeName="years")
  expect_null(res_sig$res)

  myTB22  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    90,   998,  1250, 332,
    88,   NA, 868, 578,
    89,   1150, 978, 682,
    91,  1600,  1350, 802
  )
  res_sig <- beta_conv(myTB22,89,90, TRUE, timeName="years")
  expect_null(res_sig$res)

  myTB23  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    90,   998,  1250, 332,
    88,   1201, 868, 578,
    89,   1150, 978, 682,
    91,  1600,  1350, 802
  )
  res_sig <- beta_conv(myTB23,90,89, TRUE, timeName="years")
  expect_null(res_sig$res)

})




test_that("Beta convergence, after division by tau.", {
   soDat <- tibble::tribble(
     ~years, ~UK, ~DE, ~IT,
     90,   998,  1250, 332,
     88,   1201, 868, 578,
     89,   1150, 978, 682,
     91,  1600,  1350, 802
   )[c(2,3,1,4),]
  # beta conv
  # debug(beta_conv)

  #res_sigori <- beta_conv(soDat,88,90,FALSE,timeName="years")
  #res_sig <- beta_conv(myTB2,88,90, FALSE,timeName="years")

  # order of times does not matter
  #expect_equal(res_sig$res$summary$coefficients[2,1],
  #       res_sigori$res$summary$coefficients[2,1]
  #       )

   res_notau <- beta_conv(soDat,88,91, FALSE, timeName="years",useTau=FALSE)
   res_tau <- beta_conv(soDat,88,91, FALSE, timeName="years", useTau=TRUE)

   expect_equal( res_notau$res$model$coefficients[1]/3,
                 res_tau$res$model$coefficients[1]);
   expect_equal( res_notau$res$model$coefficients[2]/3,
                 res_tau$res$model$coefficients[2]);

  # too sharp  res_tau$res$model$coefficients == res_notau$res$model$coefficients/(91-88)
})







##########################################################################
context("Check calculations of convergence: delta.")

# debug(delta_conv)

test_that("Delta convergence selection", {
  myTB2  <- emp_20_64_MS
  res_delta <- delta_conv(myTB2,time_0=2005, time_t=2010)

  #
  expect_equal(as.numeric(res_delta$res[1 ,1]), 2005)
  expect_equal(as.numeric(res_delta$res[6 ,1]), 2010)


})



test_that("Delta convergence with time present", {
  myTB2  <- tibble::tribble(
    ~time, ~UK, ~DE, ~IT,
     88,   1201, 868, 578,
     89,   1150, 978, 682,
     90,   998,  1250, 332
  )
  dat.1 <- c(1201, 868, 578)
  dat.2 <- c(1150, 978, 682)
  dat.3 <- c(998,  1250, 332)
  ma.1 <- max(dat.1)
  ma.2 <- max(dat.2)
  ma.3 <- max(dat.3)
  res_loc <- rbind(
     sum(ma.1- dat.1 ),
     sum(ma.2- dat.2 ),
     sum(ma.3- dat.3 ))
  #
  res_delta <- delta_conv(myTB2)

  #
  expect_equal(as.numeric(res_delta$res[1 ,2]), res_loc[1,1])
  expect_equal(as.numeric(res_delta$res[2 ,2]), res_loc[2,1])
  expect_equal(as.numeric(res_delta$res[3 ,2]), res_loc[3,1])
})


test_that("Delta convergence with scrambled time order (present)", {
  myTB2  <- tibble::tribble(
    ~time, ~UK, ~DE, ~IT,
    90,   998,  1250, 332,
    88,   1201, 868, 578,
    89,   1150, 978, 682
  )
  dat.1 <- c(1201, 868, 578)
  dat.2 <- c(1150, 978, 682)
  dat.3 <- c(998,  1250, 332)
  ma.1 <- max(dat.1)
  ma.2 <- max(dat.2)
  ma.3 <- max(dat.3)
  res_loc <- rbind(
    sum(ma.1- dat.1 ),
    sum(ma.2- dat.2 ),
    sum(ma.3- dat.3 ))
  res_delta <- delta_conv(myTB2)
  expect_equal(as.numeric(res_delta$res[1 ,2]), res_loc[1,1])
  expect_equal(as.numeric(res_delta$res[2 ,2]), res_loc[2,1])
  expect_equal(as.numeric(res_delta$res[3 ,2]), res_loc[3,1])
})


test_that("Delta convergence time ordered but without time variable", {
  myTB2  <- tibble::tribble(
      ~UK, ~DE, ~IT,
        1201, 868, 578,
        1150, 978, 682,
        998,  1250, 332
  )

  res_delta <- delta_conv(myTB2)
  expect_false(is.null(res_delta$err))

  myDF <- data.frame(
    ##time = c(99,97,98),
    veval1 = c(1201,1200,998),
    veval2 = c(1201,1200,998)
  );

  res_delta <- delta_conv(myDF)
  expect_false(is.null(res_delta$err))

})


test_that("Delta convergence,  scrambled time and  different name.", {
  myTB2  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    90,   998,  1250, 332,
    88,   1201, 868, 578,
    89,   1150, 978, 682
  )
  dat.1 <- c(1201, 868, 578)
  dat.2 <- c(1150, 978, 682)
  dat.3 <- c(998,  1250, 332)
  ma.1 <- max(dat.1)
  ma.2 <- max(dat.2)
  ma.3 <- max(dat.3)
  res_loc <- rbind(
    sum(ma.1- dat.1 ),
    sum(ma.2- dat.2 ),
    sum(ma.3- dat.3 ))
  res_delta <- delta_conv(myTB2,timeName="years")
  expect_equal(as.numeric(res_delta$res[1 ,2]), res_loc[1,1])
  expect_equal(as.numeric(res_delta$res[2 ,2]), res_loc[2,1])
  expect_equal(as.numeric(res_delta$res[3 ,2]), res_loc[3,1])
})


#  debug(delta_conv)

test_that("Delta convergence,  highBest and lowBest.", {
  myTB2.L  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    90,   998,  1250, 332,
    88,   1201, 868, 578,
    89,   1150, 978, 682
  )
  massimoL <- max(myTB2.L[,-1])
  myTB2.L[[2]]  <- massimoL-  myTB2.L[[2]]
  myTB2.L[[3]]  <- massimoL-  myTB2.L[[3]]
  myTB2.L[[4]]  <- massimoL-  myTB2.L[[4]]
  myTB2 <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    90,   998,  1250, 332,
    88,   1201, 868, 578,
    89,   1150, 978, 682
  )
  #res_deltaH <- delta_conv(myTB2.H, timeName="years")
  res_deltaL <- delta_conv(myTB2.L, timeName="years")
  res_deltaLB <- delta_conv(myTB2, timeName="years", indiType = "lowBest")
  for(aux in 1:3){
    expect_true(res_deltaL$res[aux,2] == res_deltaLB$res[aux,2])
  }

})


#  debug( delta_conv)

test_that("Delta convergence,  highBest  extended.", {
  myTBL  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    90,   998,  1250, 332,
    88,   1201, 868, 578,
    89,   1150, 978, 682,
    87,   998,  1250, 332,
    91,   998,  1250, 332
  )

  res_deltaLB <- delta_conv(myTBL,
                            timeName="years",
                            indiType = "highBest",
                            time_0=88,
                            time_t=90,
                            extended=T)

  expect_true(!res_deltaLB$res$strict_conv_ini_last)
  expect_true(!res_deltaLB$res$converg_ini_last)

})





test_that("Delta convergence,  check  not extended.", {
      myTB  <- tibble::tribble(
        ~time, ~UK, ~DE, ~IT,
        88,   1201, 868, 578,
        89,   1150, 978, 682, #
        90,   998,  1250, 332 #
      )

  res_delta  <- delta_conv(myTB,
                            timeName="time",
                            indiType = "highBest",
                            time_0=89,
                            time_t=90,
                            extended=F)
  res_delta2  <- delta_conv(myTB,
                           timeName="time",
                           indiType = "lowBest",
                           time_0=89,
                           time_t=90,
                           extended=F)
  expect_equal(res_delta$res$delta , c(640,1170))
  expect_equal(res_delta2$res$delta,c(sum(c(1150, 978)- 682),
                                      sum(c(998,  1250)- 332)))

})



#  debug( delta_conv)


test_that("Delta convergence,  highBest   extended.", {
  myTBc  <- tibble::tribble(
    ~time, ~UK, ~DE, ~IT,
    88,   1200,  1100,1190,
    89,   1300,    32, 66, #
    90,   1400,  1350, 1395 #
  )

  res_delta  <- delta_conv(myTBc,
                           timeName="time",
                           indiType = "highBest",
                           time_0=88,
                           time_t=90,
                           extended=T)


  expect_true(res_delta$res$converg_ini_last)
  expect_true(res_delta$res$strict_conv_ini_last)



})


test_that("Delta convergence,  lowBest   extended.", {
  myTB  <- tibble::tribble(
    ~time, ~UK, ~DE, ~IT,
    88,   10 , 7  , 1,
    89,   10 , 1    ,  5, #
    90,    8,  7 ,   7#
  )

  res_delta  <- delta_conv(myTB,
                           timeName="time",
                           indiType = "lowBest",
                           time_0=88,
                           time_t=90,
                           extended=T)


  expect_true(res_delta$res$converg_ini_last)
  expect_true(res_delta$res$strict_conv_ini_last)

  myTB2  <- tibble::tribble(
    ~time, ~UK, ~DE, ~IT,
    88,   10 , 7  , 6,
    89,   10 , 2    ,  5, #
    90,    8,  5 ,   6#
  )

  res_delta2  <- delta_conv(myTB2,
                           timeName="time",
                           indiType = "lowBest",
                           time_0=88,
                           time_t=90,
                           extended=T)
  expect_true(res_delta2$res$converg_ini_last)
  expect_false(res_delta2$res$strict_conv_ini_last)
})


test_that("Delta convergence,  constant   extended.", {
  myTB3  <- tibble::tribble(
    ~time, ~UK, ~DE, ~IT,
    88,   10 , 7  , 6,
    89,   10 , 7    ,  6, #
    90,    10,  7 ,   6#
  )

  res_delta3  <- delta_conv(myTB3,
                            timeName="time",
                            indiType = "lowBest",
                            time_0=88,
                            time_t=90,
                            extended=T)
  expect_false(res_delta3$res$converg_ini_last)
  expect_false(res_delta3$res$strict_conv_ini_last)

  res_delta3  <- delta_conv(myTB3,
                            timeName="time",
                            indiType = "highBest",
                            time_0=88,
                            time_t=90,
                            extended=T)
  expect_false(res_delta3$res$converg_ini_last)
  expect_false(res_delta3$res$strict_conv_ini_last)
})







##########################################################################
context("Check calculations of convergence: sigma.")


test_that("Sigma convergence,  scrambled time and  different name.", {
  myTB2  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    90,   998,  1250, 332,
    88,   1201, 868, 578,
    89,   1150, 978, 682
  )
  dat.1 <- c(1201, 868, 578)
  dat.2 <- c(1150, 978, 682)
  dat.3 <- c(998,  1250, 332)
  tmp <-  c(sd(dat.1 )*(2/3)^0.5,sd(dat.2 )*(2/3)^0.5,sd(dat.3)*(2/3)^0.5)
  myTB <- tibble::tibble(
    time= c(88,89,90),
    stdDev = tmp,
    CV  =  tmp/c(mean(dat.1 ),mean(dat.2 ),mean(dat.3 )),
    mean = c(mean(dat.1 ),mean(dat.2 ),mean(dat.3 ))
    )
  res_sig <- sigma_conv(myTB2,timeName="years")
  for(auxR in 1:3){
    for(auxC in 1:4){
      expect_equal(as.numeric(myTB[auxR,auxC]),
                   as.numeric(res_sig$res[auxR,auxC]))
    }
  }
})


# debug(sigma_conv)
# undebug(sigma_conv)


test_that("Sigma convergence, with or without name.", {
  myTB2  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    88,   1201, 868, 578,
    89,   1150, 978, 682,
    90,   998,  1250, 332
  )
  res_sig <- sigma_conv(myTB2[,2:4],timeName="years")
  expect_false(is.null(res_sig$err))

  res_sig <- sigma_conv(myTB2,timeName="years")
  expect_null(res_sig$err)
  expect_null(res_sig$msg)
  expect_false(is.null(res_sig$res))


})




test_that("Sigma convergence, scrambled time, different name, subset of times.", {
  myTB2  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    90,   998,  1250, 332,
    88,   1201, 868, 578,
    89,   1150, 978, 682,
    91,   232, 225, 227,
    87,   122, 212, 154
  )
  # myTB2
  dat.1 <- c(1201, 868, 578)
  dat.2 <- c(1150, 978, 682)
  dat.3 <- c(998,  1250, 332)
  tmp <-  c(sd(dat.1 )*(2/3)^0.5,sd(dat.2 )*(2/3)^0.5,sd(dat.3)*(2/3)^0.5)
  myTB <- tibble::tibble(
    time= c(88,89,90),
    stdDev = tmp,
    CV  =  tmp/c(mean(dat.1 ),mean(dat.2 ),mean(dat.3 )),
    mean = c(mean(dat.1 ),mean(dat.2 ),mean(dat.3 ))
  )
  res_sig <- sigma_conv(myTB2,timeName="years", time_0 = 88,time_t = 90)

  for(auxR in 1:3){
    for(auxC in 1:4){
      expect_equal(as.numeric(myTB[auxR,auxC]),
                   as.numeric(res_sig$res[auxR,auxC]))
    }
  }
})







test_that("Sigma convergence missing values.", {
  myTB2  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    90,   998,  1250, 332,
    88,   1201, 868, 578,
    89,   1150, 978, NA,
    91,   232, 225, 227,
    87,   122, 212, 154
  )

  res_sig <- sigma_conv(myTB2,timeName="years", time_0 = 88,time_t = 90)
  expect_null(res_sig$res)

})












##########################################################################
context("Check calculations of convergence: gamma.")


test_that("Gamma convergence,  scrambled time and  different name.", {
  myTB2  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    90,   998,  1250, 332,
    88,   1201, 868, 578,
    89,   1150, 978, 682,
    91,  1600,  1350, 802
  )
  # gamma conv
  # debug(gamma_conv)
  res_sig <- gamma_conv(myTB2,last=91,ref=88, timeName="years")

   soDat <- matrix(
     c(88,   rank(c(1201, 868, 578)),
       89,   rank(c(1150, 978, 682)),
       90,   rank(c(998,  1250, 332)),
       91,  rank(c(1600,  1350, 802))),ncol=4,byrow=T
   )
   sumRan <- apply(soDat[,-1],2,sum)
   numerator <- pop_var(sumRan)$popvar
   denominator <- (4+1)^2 * pop_var(rank(c(1201, 868, 578)))$popvar
   KIi <- numerator / denominator;
   expect_equal(KIi,   res_sig$res )
})




test_that("Gamma convergence,  regular time  different reference time and last.", {
  myTB2  <- tibble::tribble(
    ~time, ~UK, ~DE, ~IT,
    90,   998,  1250, 332,
    88,   1201, 868, 578,
    89,   1150, 978, 682,
    91,  1600,  1350, 802
  )
  # gamma conv
  # debug(gamma_conv)
  res_sig <- gamma_conv(myTB2,last=90, ref=89)

  soDat <- matrix(
    c(88,   rank(c(1201, 868, 578)),
      89,   rank(c(1150, 978, 682)),
      90,   rank(c(998,  1250, 332))
      #91,  rank(c(1600,  1350, 802))
      ),ncol=4,byrow=T
  )
  sumRan <- apply(soDat[,-1],2,sum)
  numerator <- pop_var(sumRan)$popvar
  denominator <- (3+1)^2 * pop_var(rank(c(1150, 978, 682)))$popvar
  KIi <- numerator / denominator;
  expect_equal(KIi,   res_sig$res )

})



test_that("Gamma convergence,  wrong ref and last times.", {
  myTB2  <- tibble::tribble(
    ~time, ~UK, ~DE, ~IT,
    90,   998,  1250, 332,
    88,   1201, 868, 578,
    89,   1150, 978, 682,
    91,  1600,  1350, 802
  )
  expect_false(is.null(gamma_conv(myTB2,last=6, ref=2)$err))
  expect_false(is.null(gamma_conv(myTB2,last=90, ref=2)$err))
  expect_false(is.null(gamma_conv(myTB2,last=9, ref=88)$err))
})





test_that("Gamma convergence,  highBest and lowBest.", {
  myTB2.L  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    90,   998,  1250, 332,
    88,   1201, 868, 578,
    89,   1150, 978, 682
  )
  massimoL <- max(myTB2.L[,-1])
  myTB2.L[[2]]  <- massimoL - myTB2.L[[2]]
  myTB2.L[[3]]  <- massimoL- myTB2.L[[3]]
  myTB2.L[[4]]  <- massimoL - myTB2.L[[4]]
  myTB2.H  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    90,   998,  1250, 332,
    88,   1201, 868, 578,
    89,   1150, 978, 682
  )
  res_gamL <-  gamma_conv(myTB2.L,last=90, ref=88, timeName ="years")
  res_gamH <- gamma_conv(myTB2.H,last=90, ref=88, timeName ="years")

  expect_equal(res_gamL$res, res_gamH$res)
})














