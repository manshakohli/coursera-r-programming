pollutantmean <- function(directory, pollutant, id = 1:332) {
        csv_files <- paste(paste("./specdata/",formatC(id,width=3,flag="0"),sep=""),".csv",sep="")
        acc <- c()
        for (f in csv_files) {
                df <- read.csv(f, head=T)
                if ( pollutant == "sulfate" ) {
                        vec <- df[ !is.na(df$sulfate), c('sulfate')]
                } else if (pollutant == "nitrate") {
                        vec <- df[ !is.na(df$nitrate), c('nitrate')]
                }
                acc <- c(acc,vec)
        }
        return(round(mean(acc),digits=3))
}
