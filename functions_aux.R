
jif_jci <- biblio_cor(df, '2020 JIF', '2020 JCI')

jif_jci <- jif_jci[which(!is.na(jif_jci$Correlation)),]
jif_jci <- jif_jci[which(jif_jci$Journals>25),]

df_a <- dplyr::inner_join(df, esi_categories[,c('WC', 'ESI')], by=c('Cat'='WC'))


ggplot(data=df_a, aes(x=ESI, y=`2020 JCI` ))+
  geom_boxplot(outlier.shape=NA)+
  coord_flip()+
  ylim(c(0,2.5))+
  theme_light()+
    theme(panel.grid.major.x = element_blank())+
    theme(panel.grid = element_blank(),
          text=element_text(family='Arial', size=12.5, color='black'),
          axis.text = element_text(color='black', size=11),
          axis.ticks = element_line(color='black'),
          legend.position="bottom",
          panel.border = element_rect(colour = "black"),
          strip.background = element_rect(colour="black",
                                          fill="black"),
          strip.text = element_text(size=14) )+
    guides(fill = guide_colorbar( label.position = "bottom",
                                  title.position = "left", title.vjust = 0.85))


dim(unique(df[which(df$DB == 'SCIE'),c(1:3)]))
dim(unique(df[which(df$DB %in% c('SCIE', 'SSCI')),c(1:3)]))

df[!(duplicated(df) | duplicated(df, fromLast = TRUE)), ]



df <- as.data.frame(df)
a <- unique(df[which(df$DB %in% c('SSCI', 'SCIE')),c(1:3)])
dim(setdiff(a, df[which(!(df$DB %in% c('SSCI', 'SCIE'))),c(1:3)]))


length(unique(df[which(df$DB %in% c('ESCI')),c(1:3,23)]$Cat))

sum(!(unique(df[which(df$DB %in% c('AHCI')),c(1:3,23)]$Cat) %in% unique(df[which(!(df$DB %in% c('AHCI'))),c(1:3,23)]$Cat)))

dim(unique(df[which((df$DB %in% c('ESCI', 'SSCI')) & (!(is.na(df$`2020 JCI`)))),c(1:3)]))


dim(unique(df[which((df$DB %in% c('SSCI')) & (!(is.na(df$`2020 JCI`))) & (!(is.na(df$`2020 JIF`)))),c(1:3)]))

SCIE
SSCI