#install.packages("pdftools")
require(pdftools) 
library(pdftools)
pdf_text_data <- pdf_text("../data/Mortalidad-RegDem-2015-17-NYT-part1.pdf")
pdf_text_data

df <- data.frame(date = integer(), month=character(), year = integer(), count = integer())
for (j in 1:12) {
  f <- 0
  for (i in 4:100) {
    x <- strsplit(strsplit(pdf_text_data[j], "\n")[[1]][i]," ")[[1]]
    if(f==0 & is.na(as.integer(x[which(x!="")[1]]) )) {
      if(x[which(x!="")[1]]!="16-17") {
        month <- x[which(x!="")[1]]
        f <- 1
        print(month)
      }
    }
    if(f==1) {
      if(x[which(x!="")[1]]=='Total') {
        break
      }
      if(which(x!="")[1]>25 | is.na(as.integer(x[which(x!="")[1]]))) {
        next
      }
      date <- x[which(x!="")][1]
      y_2015 <- x[which(x!="")][2]
      y_2016 <- x[which(x!="")][3]
      y_2017 <- x[which(x!="")][4]
      df <- rbind(df, data.frame(date = date, month = month, year = 2015, count = y_2015))
      df <- rbind(df, data.frame(date = date, month = month, year = 2016, count = y_2016))
      df <- rbind(df, data.frame(date = date, month = month, year = 2017, count = y_2017))
    }
  }
}
df <- df[!(df$month=='DEC'& df$year==2017),]
write.csv(df, "../data/excess_mort_.csv")

