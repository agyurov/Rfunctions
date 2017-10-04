remove(list = ls())
setwd("d:/google drive/projects_personal/getoldtweets-java")

df = read.csv('alldata_fixed_encoding.csv', header = T, stringsAsFactors = F, sep = ',')

tweets = df$text
# A function to scan tweets for http and https and split these
simple_split = function(x){
  x = strsplit(x, 'http', fixed = T)
  out = do.call(rbind, x)
  out[,2] = c(paste0('http', out[,2]))
  out = data.frame(out)
  names(out) = c('tweet', 'linkz')
  return(out)
}


df2 = df

tmp = simple_split(df2$text)
