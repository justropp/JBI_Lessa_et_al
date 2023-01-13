# JBI_Lessa_et_al

# Data and figure for "How taxonomic change influences forecasts of the Linnean Shortfall" (Lessa et al. under review)

### Download and manipulate data for Fagaceae and Solanaceae

require(kewr, dplyr, tidyr, ggplot, viridis)

wcvp_names<-download_wcvp() # download World Checklist of Selected Plant Families. Download can alse be dome manualy from http://sftp.kew.org/pub/data-repositories/WCVP/

wcvp_names<-read.table("wcvp_names.csv", sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")

wcvp_names$first_published2 <- gsub("[()]", "", kew2$first_published) # exclude parenthesis

wcvp_names$first_published2<-as.numeric(wcvp_names$first_published2) # Store publication year as a numerical variable

head(wcvp_names)

#### Select Fagaceae

faga<-subset(wcvp_names, wcvp_names$family == "Fagaceae" & wcvp_names$taxon_rank == "Species")

faga2 <- faga %>% group_by(first_published2,taxon_status) %>% tally() ## count how many names described each year is associated with each taxonomic status (e.g., accepted, synonym, etc.)

faga3<-as.data.frame(spread(faga2, key = taxon_status, value = n)) ## change data from long to wide format

faga3$N_desc <- rowSums(faga3[2:ncol(faga3)], na.rm = TRUE) ## count total number of description each year

faga3$prop_acc<-faga3$Accepted/faga3$N_desc ## calculate proportion of accepted

faga3$years_since_desc<-2022-faga3$first_published2 ## years since description

### Plot
p<-ggplot(faga3, aes(years_since_desc, prop_acc, color = N_desc)) + geom_point(size = 4, alpha = 0.8) + 

geom_smooth(method = "loess", color = "black", size = 0.8)+ 

ylim(0,1)+ 

scale_colour_viridis(limits = c(1, 250), option = "viridis", trans = "log10") +

theme_light()+ labs(y = "Porportion of accepted species", x = "Years since first description")

p + theme( #plot.title = element_text(color="grey", size=14, face="bold.italic"), 

axis.title.x = element_text(color="grey21", size=18), 

axis.title.y = element_text(color="grey21", size=18), 

axis.text.x = element_text(color="grey21", size=18), 

axis.text.y = element_text(color="grey21", size=18) )


### Solanaceae

sola<-subset(wcvp_names, wcvp_names$family == "Solanaceae " & wcvp_names$taxon_rank == "Species") # then follow tha same steps described above

### Characidae
Data was downloaded from Froese, R., & Pauly., D. (2022). FishBase version 08/2022; www.fishbase.org.
