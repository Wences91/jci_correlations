require(tidyr)

read_jcr <- function(path){
  
  files <- dir(path=path)
  
  for(i in 1:length(files)){
    
    df_aux <- read.csv(text=paste0(head(readLines(paste0(path, files[i])), -3), collapse='\n'),
                       stringsAsFactors=FALSE,
                       na.strings = c('N/A', 'n/a'),
                       skip=2,
                       check.names=FALSE,
                       row.names=NULL,
                       header=TRUE,
                       quote='"',
                       encoding='UTF-8')
    
    colnames(df_aux)[1:(dim(df_aux)[2]-1)] <- colnames(df_aux)[2:dim(df_aux)[2]]
    df_aux <- df_aux[,1:(dim(df_aux)[2]-1)]
    
    if(i == 1){
      df <- df_aux
    }else{
      df <- rbind.data.frame(df, df_aux, stringsAsFactors=FALSE)
    }
  }
  
  # Minor errors with commas and %
  df$`Total Citations` <- as.integer(gsub(',', '', df$`Total Citations`))
  df$`% of OA Gold` <- as.numeric(gsub('%', '', df$`% of OA Gold`))
  df$`Total Articles` <- as.integer(gsub(',', '', df$`Total Articles`))
  df$`% of Articles in Citable items` <- as.numeric(gsub('%', '', df$`% of Articles in Citable items`))
  df$`Citable Items` <- as.integer(gsub(',', '', df$`Citable Items`))
  
  # Separate journals by categories
  df <- separate_rows(df, Category, sep = '; ', convert = FALSE)
  
  # Create columns for the specific category and index
  df$DB <- gsub('.* - ', '', df$Category)
  df$Cat <- gsub(' -.*', '', df$Category)
  
  df <- unique(df)
  
  print(paste('There are', dim(unique(df[,c('ISSN', 'eISSN')]))[1], 'journals'))
  print(paste('There are', dim(unique(df[,c('Cat')]))[1], 'categories'))
  
  return(df)
}


biblio_cor <- function(df, indicator_1, indicator_2){
  
  df_cor <- data.frame(Category=unique(df$Category),
                       Correlation=NA,
                       DB=NA,
                       Cat=NA,
                       Journals=NA,
                       stringsAsFactors = FALSE)
  
  # NAs rows are removed
  df <- df[which(!is.na(df[, indicator_1]) & !is.na(df[, indicator_2])),]
  
  for(i in unique(df$Category)){
    df_cor[which(df_cor$Category==i),'Correlation'] <- cor(df[which(df$Category==i), indicator_1], df[which(df$Category==i), indicator_2], method='pearson')
    
    df_cor[which(df_cor$Category==i),'DB']  <- unique(df[which(df$Category==i), 'DB'])
    df_cor[which(df_cor$Category==i),'Cat'] <- unique(df[which(df$Category==i), 'Cat'])
    
    df_cor[which(df_cor$Category==i),'Journals'] <- dim(df[which(df$Category==i),])[1]
  }
  
  return(df_cor)
}

