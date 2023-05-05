runScript <- function(input)  {
  #Luetaan tiedosto ja tarvittaessa muutetaan se .txt-muotoon
  file <- getfile(input)

  #Luodaan taulukot metadatalle ja testidatalle, kts. apufunktiot
  metadata <- createMetadata(file)
  test.data <- createData(file)
  test.data.reduced <- reduceData(test.data)
  
  # Luodaan lineaarinen regression R:n lm-luokkaa käyttäen ja plotataan tulokset
  linmod <- lm(Load ~ HR, data=test.data.reduced)
  summ <- summary(linmod)
  if (summ$r.squared < 0.9) print(
    "WARNING: R2 below expected, check linearity assumption"
  )
  
  fitness.res <- calcParameters(summ, metadata, test.data.reduced)
  plot <- ggplotRegression(linmod, fitness.res)
  
  return(list("metadata"=metadata, "model"=linmod, "plot"=plot, 
              "results"=fitness.res))
}

runExample <- function(file = "example.txt") {
  metadata <- createMetadata(file)
  test.data <- createData(file)
  test.data.reduced <- reduceData(test.data)
  linmod <- lm(Load ~ HR, data=test.data.reduced)
  summ <- summary(linmod)
  fitness.res <- calcParameters(summ, metadata, test.data.reduced)
  plot <- ggplotRegression(linmod, fitness.res)
  return(list("metadata"=metadata, "model"=linmod, "plot"=plot, 
              "results"=fitness.res))
}

#PALAUTTAA TIEDOSTON
getfile <- function(input) {
  file <- input$filinp$datapath
  if (file_ext(file) != "txt") {
    newfile <- paste0(substring(file,1,nchar(file)-3),"txt")
    if (!file.exists(newfile)){
      file.copy(file, newfile,overwrite = TRUE)
    }
    file.remove(file)
    file <- newfile
    rm(newfile)
  }
  return(file)
}

#Laskee max- ja submax-arvot regressioyhtälöstä ja rasitusdatasta
calcParameters <- function (summ, metadata, test.data.reduced) {
  # Tämä funktio laskee tehon regressioyhtälön perusteella
  predictor <- function(HR) {
    load = summ$coefficients[1] + summ$coefficients[2]*HR
    return(load)
  }
  # TÄMÄ FUNKTIO LASKEE VO2MAX:n TEHON PERUSTEELLA
  vo2max_calc <- function(P) {
    wr = 6.12*P
    return((1.8*wr)/metadata$Weight + 7)
  }
  submax <- 0.60
  #Luodaan lista parametreille
  fitness.res <- list()
  #Lasketaan iänmukainen maksimi-HR, maksimiteho ja VO2max
  fitness.res["maxHR"] <- 220 - metadata$Age
  fitness.res["Powermax"] <- predictor(fitness.res$maxHR)
  fitness.res["VO2max"] <- vo2max_calc(fitness.res$Powermax)
  #Etsitään stage joka on lähimpänä ~60% maksimitehoa
  fitness.res["Powersubmax"] <- unique(test.data.reduced$Load)[which.min(abs(unique(test.data.reduced$Load)-(submax*fitness.res$Powermax)))]
  #Otetaan datasta HR, VO2, RER ja GE-keskiarvot tältä stagelta
  fitness.res["HRsubmax"] <- mean(subset(test.data.reduced, subset = test.data.reduced$Load ==fitness.res$Powersubmax)$HR)
  fitness.res["V02submax"] <- mean(subset(test.data.reduced, subset = test.data.reduced$Load ==fitness.res$Powersubmax)$V.O2)/1000
  fitness.res["RERsubmax"] <- mean(subset(test.data.reduced, subset = test.data.reduced$Load ==fitness.res$Powersubmax)$RER)
  fitness.res["GEsubmax"] <- mean(subset(test.data.reduced, subset = test.data.reduced$Load ==fitness.res$Powersubmax)$GE)
  return(fitness.res)
}

#Tämä funktio lukee tekstitiedostosta metadatat
createMetadata <- function (file) {
  raw <- read.delim(file, header = FALSE, 
                    fill = TRUE,)
  metadata <- list()
  for (v in c("Last Name", "First Name", "Date of Birth", "Identification","Gender")) {
    c <- ceiling(which(raw == paste0(v,":"))/nrow(raw))
    r <- which(raw == paste0(v,":")) %% nrow(raw)
    metadata[v] <- raw[r,c+1]
    rm(r,c)
  }
  for (v in c("Age","Height","Weight","BMI")) {
    c <- ceiling(which(raw == paste0(v,":"))/nrow(raw))
    r <- which(raw == paste0(v,":")) %% nrow(raw)
    metadata[v] <- as.numeric(strsplit(raw[r,c+1], " ")[[1]][1])
    rm(r,c)
  }
  return(metadata)
}

#Tämä funktio etsii mittausdatan alkukohdan,
#sen jälkeen se etsii missä vaiheessa testivaihe alkaa ja lukee sen taulukoksi
createData <- function (file) {
  raw <- read.delim(file, header = FALSE, 
                    fill = TRUE,)
  data <- read.delim(file, header = TRUE, 
                     skip = which(raw[1] == "Time"),)
  test.data <- data[-1:-which(data$t.ph == "Test",),]
  test.data$Load <- as.numeric(test.data$Load)
  test.data <- test.data[1:max(which(test.data$Load == max(test.data$Load))),]
  test.data$RER <- as.numeric(test.data$RER)
  test.data$HR <- as.numeric(test.data$HR)
  test.data$V.O2 <- as.numeric(test.data$V.O2)
  test.data["EEJs"] <- (as.numeric(test.data$EE.d) * 4184)/86400
  test.data["GE"] <- test.data$Load/test.data$EEJs*100
  return(test.data)
}

#Tämä funktio poistaa ensimmäisen minuutin joka stagelta, jolloin saadaan
#regressioon kelpaava data
reduceData <- function (test.data) {
  test.data.reduced <- data.frame()
  for (l in unique(test.data$Load)) {
    le = length(which(test.data$Load == l))
    i = which(test.data$Load == l)[ceiling(le/2):le]
    test.data.reduced <- rbind(test.data.reduced, test.data[i,])
  }
  return(test.data.reduced)
}

#Tämä on netistä näpätty näppärä funktio 
#joka plottaa regressiokäyrän ja näyttää sen parametrit
ggplotRegression <- function (fit, results) {
  
  require(ggplot2)
  
  maxHR <- results$maxHR
  maxP <- results$Powermax
  submaxHR <- results$HRsubmax
  submaxP <- results$Powersubmax
  
  ggplot(fit$model, aes(x = !! sym(names(fit$model)[2]), y = !! sym(names(fit$model)[1]))) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red", formula = y~x) +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5))) +
                       geom_hline(yintercept = maxP, linetype="dashed", color="red") +
                       geom_vline(xintercept = maxHR, linetype="dashed", color="red") +
                       geom_hline(yintercept = submaxP, linetype="dashed", color="green") +
                       geom_vline(xintercept = submaxHR, linetype="dashed", color="green") +
                       theme(text = element_text(size = 20))
}
