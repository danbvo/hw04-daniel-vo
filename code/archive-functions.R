library(XML)
library(stringr)
library(ggplot2)
library(dplyr)

setwd("/Users/danbvo/Desktop/Academic-Portfolio/Stat-133/HW/hw04/data")

tbl_html <- data.frame(readHTMLTable('http://cran.r-project.org/src/contrib/Archive/stringr'))
tbl_html <- na.omit(tbl_html)
tbl_html <- subset(tbl_html, select = -c(1))

colnames(tbl_html) <- c('name', ' last modified', 'size', ' description')

tbl_html

read_archive <- function(x){
  master_url <- "http://cran.r-project.org/src/contrib/Archive/"
  package <- data.frame(readHTMLTable(paste(master_url,x, sep = "")))
  package <- na.omit(package)
  package <- subset(package, select = -c(1))
  colnames(package) <- c('name', ' last modified', 'size', ' description')
  return(package)
}
x <- read_archive("ggplot2")


#
clean_archive <- function(x){
  splitup <- str_split(x$name, pattern = "_")
  namecol <- matrix("", nrow = length(splitup), ncol = 1)
  for (i in 2:length(splitup)){
    namecol[i,] <- splitup[[i]][1]
  }
  name <- namecol
  verscol <- matrix("", nrow = length(splitup), ncol = 1)
  for (i in 2:length(splitup)){
    verscol[i,] <- splitup[[i]][2]
  }
  version <- str_sub(verscol, end = -8)
  date <- str_sub(x$` last modified`, start = 1, end = 10)
  type <- str_detect(x$size, "M")
  sizeM <- suppressWarnings(as.numeric(str_replace(x$size, "M", ""))*1000)
  sizeK <- suppressWarnings(as.numeric(str_replace(x$size, "K", "")))
  size <- ifelse(type, sizeM, sizeK)
  clean_package <- data.frame(name, version, date, size)
  clean_package <- clean_package[-1,]
  return(clean_package)
}

raw <- read_archive("XML")
clean_data <- clean_archive(raw)



write.csv(clean_data, file = "stringr-archive.csv")

plot_archive <- function(clean_data){
  clean_data <- arrange(clean_data, clean_data$date)
  clean_data$date <- as.Date(clean_data$date)
  ggplot(data = clean_data, aes(date, size)) +
    geom_step() +
    geom_point()
}

raw <- read_archive("dplyr")
clean_data <- clean_archive(raw)
plot_archive(clean_data)

# 1.5
clean_dplyr <- clean_archive(read_archive("dplyr"))
write.csv(clean_dplyr, file = "dplyr-archive.csv")
clean_knitr <- clean_archive(read_archive("knitr"))
write.csv(clean_knitr, file = "knitr-archive.csv")
clean_ggplot2 <- clean_archive(read_archive("ggplot2"))
write.csv(clean_ggplot2, file = "ggplot2-archive.csv")
clean_xml <- clean_archive(read_archive("XML"))
write.csv(clean_xml, file = "xml-archive.csv")

table <- rbind(clean_dplyr, clean_knitr, clean_ggplot2, clean_xml)
table$date <- as.Date(table$date)

ggplot(table, aes(date, size, color = name)) +
  geom_step(direction = "hv")

ggplot(table, aes(date, size, color = name)) +
  geom_step(direction = "hv") + 
  facet_wrap(~name, scales = "free")
