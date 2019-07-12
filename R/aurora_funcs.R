library(jpeg)
library(mailR)
library(rpart)

#' @export
compress <- function(x, scaledown=2) {
  maxx <- dim(x)[2]
  maxy <- dim(x)[1]
  xlo <- seq(1,maxx,by=scaledown)
  xhi <- seq(scaledown,maxx,by=scaledown)
  ylo <- seq(1,maxy,by=scaledown)
  yhi <- seq(scaledown,maxy,by=scaledown)
  xx <- array(dim=c(length(ylo), length(xlo), 3))
  for(k in 1:3) {
    for(j in 1:length(xlo)) {
      for(i in 1:length(ylo)) {
        xx[i,j,k] <- mean(x[ylo[i]:yhi[i],xlo[j]:xhi[j],k])
      }
    }
  }
  return(xx)
}

#' @importFrom jpeg readJPEG
#' @export
pull_the_last_allsky <- function() {
  rawpage <- tryCatch(readLines("http://allsky.gi.alaska.edu/tagged_cam/"), error=function(x) "")
  possible_filenames <- substr(rawpage,81,96)
  possible_filenames <- possible_filenames[substr(possible_filenames,13,16)==".jpg"]
  filename <- possible_filenames[length(possible_filenames)]
  if(length(filename)>0) {
    z <- tempfile()
    filename1 <- paste0("http://allsky.gi.alaska.edu/tagged_cam/",filename)
    download.file(filename1, destfile=z, mode="wb")
    rawpic <- tryCatch(readJPEG(z), error=function(x) NA)
  } else {
    rawpic <- NA
  }
  return(rawpic)
}

#' @importFrom jpeg readJPEG
#' @export
pull_allsky <- function(npull=150, start=20, end=6) {
  hrs <- c(start:23,paste0("0",0:end))
  rawpage <- readLines("http://allsky.gi.alaska.edu/tagged_cam/")
  possible_filenames <- substr(rawpage,81,96)
  possible_filenames <- possible_filenames[substr(possible_filenames,13,16)==".jpg"]
  allhrs <- substr(rawpage,160,161)
  possible_filenames <- possible_filenames[allhrs %in% hrs]
  whichones <- round(seq(1,length(possible_filenames),length.out=npull))
  filenames <- possible_filenames[whichones]
  allpics <- list()
  for(i in 1:length(filenames)) {
    filename <- paste0("http://allsky.gi.alaska.edu/tagged_cam/",filenames[i])
    z <- tempfile()
    asdf <- tryCatch(download.file(filename, destfile=z, mode="wb"), error=function(x) NA)
    if(!is.na(asdf)) allpics[[i]] <- tryCatch(jpeg::readJPEG(z), error=function(x) NA)
    file.remove(z)
    print(paste("downloaded",i,"of",npull))
  }
  return(allpics)
}
# allpics_226 <- pull_allsky()

#' @importFrom jpeg readJPEG
#' @export
pull_murray <- function() {
  z <- tempfile()
  download.file("http://live.alaskaauroracam.com/livejpg.php", destfile=z, mode="wb")
  rawpic <- tryCatch(readJPEG(z), error=function(x) NA)
  file.remove(z)
  return(rawpic)
}

#' @importFrom graphics rasterImage
#' @export
plotpic <- function(x, ...) {
  plot(NA,xlim=0:1,ylim=0:1,...=...)
  graphics::rasterImage(x,0,0,1,1)
}

#' @export
plot_pics <- function(x, func=NULL) {
  for(i in 1:length(x)) {
    plotpic(x[[i]], main=i)
    if(!is.null(func)) plotpic(func(x[[i]]), main=i)
  }
}

#' @export
pospart <- function(x) {
  x[x<0] <- 0
  return(x)
}

#' @export
greenness <- function(x) pospart(x[,,2]-.5*(x[,,1])-.5*(x[,,3]))

#' @export
meangreen <- function(x) mean(greenness(x))

#' @export
meangreen_thresh <- function(x, thresh) mean(greenness(x)>=thresh)

#' @export
sumcontrast <- function(x) {
  g <- greenness(x)
  v <- g[-1,]-g[-nrow(g),]
  h <- g[,-1]-g[,-ncol(g)]
  sumthing <- sum(abs(v)) + sum(abs(h))
  return(sumthing)
}

#' @export
sumcontrast2 <- function(x) {
  g <- greenness(x)
  v <- g[-1,]-g[-nrow(g),]
  h <- g[,-1]-g[,-ncol(g)]
  sumthing <- sum(v^2) + sum(h^2)
  return(sumthing)
}

#' @importFrom graphics rasterImage
#' @importFrom grDevices adjustcolor
#' @importFrom grDevices rainbow
#' @importFrom graphics abline
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics points
#' @importFrom graphics plot
#' @export
plot_pics_df <- function(x, df, pred=NULL) {
  mins <- matrix(apply(df,2,min,na.rm=T),nrow=nrow(df),ncol=ncol(df),byrow=T)
  maxs <- matrix(apply(df,2,max,na.rm=T),nrow=nrow(df),ncol=ncol(df),byrow=T)
  df1 <- (df-mins)/(maxs-mins)
  cols <- adjustcolor(rainbow(ncol(df)), red.f=.9, green.f=.9, blue.f=.9)
  if(!is.null(pred)) preds <- predict(pred, newdata=df)
  for(i in 1:length(x)) {
    plot(NA,xlim=0:1,ylim=0:1, main=i)
    graphics::rasterImage(x[[i]],0,0,1,1)
    plot(NA,xlim=c(1,nrow(df)), ylim=0:1, main=ifelse(is.null(pred),"",round(preds[i],2)))
    for(j in 1:ncol(df)) {
      points(df1[,j], col=cols[j])
      lines(df1[,j], col=cols[j])
      points(i, df1[i,j], col=cols[j], pch=16)
    }
    abline(h=df1[i,],col=cols)
    legend("topright",pch=16,col=cols,legend=names(df))
  }
}

#' @importFrom stats predict
#' @export
plot_pics_pred <- function(x, predmodel, pred_df, func=NULL) {
  preds <- predict(predmodel, newdata=pred_df)
  for(i in 1:length(x)) {
    plotpic(x[[i]], main=c(i, round(preds[i],2)))
    if(!is.null(func)) plotpic(func(x[[i]]), main=i)
  }
}

#' @importFrom mailR send.mail
#' @export
send_email <- function(mailto, mailuser, mailpw, sub,bod=" ") {
  send.mail(from = mailuser,
                                              to = mailto,
                                              subject = sub,
                                              body = bod,
                                              smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = mailuser, passwd = mailpw, ssl = TRUE),
                                              authenticate = TRUE,
                                              send = TRUE)
}

# send_email(paste0("It's level ",4,", boss."))
# email_level <- function(x) send_email(paste0("It's level ",x,", boss."))

#' @export
summary_df_murray <- function(x) {
  x1 <- x[!is.na(x)]
  x2 <- lapply(x1, function(xx) xx[1:675,,])
  df <- data.frame(bright=sapply(x2, meangreen),
                   thr1=sapply(x2, meangreen_thresh, thresh=.1),
                   thr2=sapply(x2, meangreen_thresh, thresh=.2),
                   thr3=sapply(x2, meangreen_thresh, thresh=.3),
                   thr4=sapply(x2, meangreen_thresh, thresh=.4),
                   contr=sapply(x2, sumcontrast),
                   contr2=sapply(x2, sumcontrast2))
}

#' @export
summary_df_allsky <- function(x) {
  x2 <- x[!is.na(x)]
  # x2 <- lapply(x1, function(xx) xx[1:675,,])
  df <- data.frame(bright=sapply(x2, meangreen),
                   thr1=sapply(x2, meangreen_thresh, thresh=.1),
                   thr2=sapply(x2, meangreen_thresh, thresh=.2),
                   thr3=sapply(x2, meangreen_thresh, thresh=.3),
                   thr4=sapply(x2, meangreen_thresh, thresh=.4),
                   contr=sapply(x2, sumcontrast),
                   contr2=sapply(x2, sumcontrast2))
}




## --------------------------------------------------------


# n <- 160           # how many images to look at
# sleeptime <- 60*5  # how long to wait between images
#
# n*sleeptime/60/60  # it will run for this many hours
#
# filename <- "415"  # date
#
#
#
# setwd("C:/Users/mbtyers/Documents/nlight_nnet")
# # setwd("D:/")
# load(file="nlights_all.Rdata")


#' @export
run_all_night <- function(n=60, sleeptime=60*5, filename=as.character(Sys.Date()),
                          mailto, mailuser, mailpw,
                          save_murray=T, maxlevel=1.5) {
murray_pics <- list()
murray_maxlevel <- allsky_maxlevel <- maxlevel   # can make this a larger number for later min alert
for(i in 1:n) {
  Sys.sleep(sleeptime)
  murray_pic <- pull_murray()
  allsky_pic <- pull_the_last_allsky()

  murray_pics[[i]] <- murray_pic

  # can take this out if you don't want it to store the image data
  if(save_murray) save(murray_pics,file=paste0("murray_",filename,".Rdata"))

  pred_murray <- NA
  if(!all(is.na(murray_pic))) {
    picnow <- murray_pic[1:675,,]
    dfnow <- data.frame(bright=meangreen(picnow),
                        thr1=meangreen_thresh(picnow, thresh=.1),
                        thr2=meangreen_thresh(picnow, thresh=.2),
                        thr3=meangreen_thresh(picnow, thresh=.3),
                        thr4=meangreen_thresh(picnow, thresh=.4),
                        contr=sumcontrast(picnow),
                        contr2=sumcontrast2(picnow))


    pred_murray <- predict(murray_part, newdata=dfnow)

    if(pred_murray > murray_maxlevel) {
      murray_maxlevel <- pred_murray
      send_email(mailto=mailto, mailuser=mailuser, mailpw=mailpw,
                 paste("Murray at",round(pred_murray,1)),
                 "http://live.alaskaauroracam.com/livejpg.php")
    }
  }

  pred_allsky <- NA
  if(!all(is.na(allsky_pic))) {
    picnow <- allsky_pic
    dfnow <- data.frame(bright=meangreen(picnow),
                        thr1=meangreen_thresh(picnow, thresh=.1),
                        thr2=meangreen_thresh(picnow, thresh=.2),
                        thr3=meangreen_thresh(picnow, thresh=.3),
                        thr4=meangreen_thresh(picnow, thresh=.4),
                        contr=sumcontrast(picnow),
                        contr2=sumcontrast2(picnow))


    pred_allsky <- predict(allsky_part, newdata=dfnow)

    if(pred_allsky > allsky_maxlevel) {
      allsky_maxlevel <- pred_allsky
      send_email(mailto=mailto, mailuser=mailuser, mailpw=mailpw,
                 paste("Allsky at",round(pred_allsky,1)),
                 "http://allsky.gi.alaska.edu/")
    }
  }

  print(c(i, pred_murray, pred_allsky))
}
}

# murray_df_new <- summary_df_murray(murray_pics)
# preds1 <- predict(murray_part, newdata=murray_df_new)
# table(preds1)
# par(mfrow=c(2,2))
# murray_pics1 <- murray_pics[!is.na(murray_pics)]
#
# # review all the Murray cam images, with model-predicted ratings
# plot_pics_pred(murray_pics1, murray_part, murray_df_new)
#
# # the 409 murray images had some purple - try to find the color space
#
#
# # # rate all night's images to update the model...
#
# # therating <- c(rep(0,117),1,0,0,1,1,1,2,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,2,2,1,2,2,2,1,2,3,2,1,3,1,0)
# # length(therating)
# # murray_df_new$rating <- therating
# # murray_df_new$date <- filename
# # #
# # murray_df <- rbind(murray_df, murray_df_new)
#
#
# # download all night's Allsky images
# allsky_pics <- pull_allsky()
# allsky_df_new <- summary_df_allsky(allsky_pics)
# allsky_pics1 <- allsky_pics[!is.na(allsky_pics)]
# preds1 <- predict(allsky_part, newdata=allsky_df_new)
# table(preds1)
# par(mfrow=c(2,4))
#
# # review all the Murray cam images, with model-predicted ratings
# plot_pics_pred(allsky_pics1, allsky_part, allsky_df_new)
#
# # par(mfrow=c(2,4))
# # plot_pics_df(allsky_pics1, allsky_df_new, allsky_part)
#
# # # save the Allsky images
# save(allsky_pics1, file="allsky_415.Rdata")
#
#
# # # rate all night's images to update the model...
# therating <- c(rep(0,55),1,0,0,0,1,0,0,2,1,4,3,4,4,3,3,3,3,
#                2,1,2,1,2,1,0,0,0,0,0,0,0,1,2.5,2,3,3,3,2,1,1,0,1,
#                0,0,0,0,2,1,0,0,rep(0,45))
# length(therating)
# allsky_df_new$rating <- therating
# allsky_df_new$date <- 415
# #
# allsky_df <- rbind(allsky_df, allsky_df_new)
#
# # update the model with new ratings and save results...
# allsky_part <- rpart(rating~bright+thr1+thr2+thr3+thr4+contr+contr2, data=allsky_df)
# murray_part <- rpart(rating~bright+thr1+thr2+thr3+thr4+contr+contr2, data=murray_df)
#
# save(allsky_df, murray_df, allsky_part, murray_part, file="nlights_all.Rdata")
#
#
# # plot stuff!!
# library(partykit)
# par(mfrow=c(1,1))
# plot(as.party(allsky_part))
# plot(as.party(murray_part))
#
# table(allsky_df$rating)
# table(murray_df$rating)
#
# par(mfrow=c(2,2))
# plot(jitter(allsky_df$rating), predict(allsky_part))
# boxplot(predict(allsky_part)~allsky_df$rating)
# plot(jitter(murray_df$rating), predict(murray_part))
# boxplot(predict(murray_part)~murray_df$rating)
#
#
# allsky_part1 <- rpart(rating~bright+thr1+thr2+thr3+thr4+contr+contr2, data=allsky_df, minbucket=4, minsplit=8)
# #                       weights=1+allsky_df$rating!=0)
# murray_part1 <- rpart(rating~bright+thr1+thr2+thr3+thr4+contr+contr2, data=murray_df, minbucket=4, minsplit=8)
# #                       weights=1+murray_df$rating!=0)
# #
# par(mfrow=c(1,1))
# plot(as.party(allsky_part1))
# plot(as.party(murray_part1))
#
#
# par(mfrow=c(2,2))
# plot(jitter(allsky_df$rating), predict(allsky_part1))
# boxplot(predict(allsky_part1)~allsky_df$rating)
# plot(jitter(murray_df$rating), predict(murray_part1))
# boxplot(predict(murray_part1)~murray_df$rating)




