setwd("d:/google drive/projects_personal/getoldtweets-java")

politicians = read.csv('politicians.csv', header = T)
collected = list.files('d:/google drive/projects_personal/getoldtweets-java/data',
                       pattern = '*.csv')
collected = gsub('.csv', '', collected)
remaining = !politicians$screen_name %in% collected
remaining = politicians$screen_name[remaining]


remaining = data.frame(screen_name = as.character(remaining))
write.csv(remaining, 'politicians2.csv', row.names = F)
