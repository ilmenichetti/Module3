
# this file is meant just to build up the basic analysis on the data we want the students to perform
# I will then corrupt the datafile
library(RColorBrewer)
## Add an alpha value to a colour
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}


## Data read
emissions<-read.csv("co2_emissions_tonnes_per_person.csv")
population_dens<-read.csv("population_density_per_square_km.csv")
population<-read.csv("population_total.csv")
deaths<-read.csv("time_series_covid19_deaths_global.csv")
metadata<-read.csv("Countries_metadata.csv")

##0 collapse the Covid data to have 1 nation only and rescale deaths by pop size
  deaths_agg<-aggregate(deaths[5:dim(deaths)[2]], list(deaths$Country.Region), sum)   # 0.0 collapse data
  population_ID<-match(deaths_agg$Group.1,population$country) #0.1 rescale deaths by population
  death_population_DF<-data.frame(deaths_agg$Group.1,population$country[population_ID], deaths_agg$X11.3.20, population$X2018[population_ID])   #0.2 build a combined data frame
  # I put both ID columns to doublecheck the matching is going fine, I will keep during the following matchings all the ID columns to see if there are problems
  death_by_population_DF<-data.frame(death_population_DF$deaths_agg.Group.1, death_population_DF$deaths_agg.X11.3.20/(death_population_DF$population.X2018.population_ID.*10^-6))   #0.3 deaths normalized per million people
  colnames(death_by_population_DF)<-c("Group.1", "Deaths_by_million")

##2  Start putting together the datasets (I do it step by step do check on the way I'm not messing up with the orders of the indexes)
  pop_ID<-match(death_by_population_DF$Group.1,population_dens$country) # 2.1 find which nations in the death table are matching in the liberty index table and relative indexes
  death_pop_DF<-data.frame(death_by_population_DF$Group.1,population_dens$country[pop_ID], death_by_population_DF$Deaths_by_million, population_dens$X2018[pop_ID])#2.2 build a combined data frame

##3 emissions + density
 emissions_ID<-match(death_pop_DF$death_by_population_DF.Group.1,emissions$country)
 emissions_death_pop_DF<-data.frame(death_pop_DF, emissions$country[emissions_ID], emissions$X2018[emissions_ID])

##4 emissions + density + metadata
 metadata_ID<-match(emissions_death_pop_DF$death_by_population_DF.Group.1,metadata$name)
 emissions_death_pop_metadata_DF<-data.frame(death_pop_DF, metadata$name[metadata_ID], metadata$World.bank.region[metadata_ID], metadata$World.bank..4.income.groups.2017[metadata_ID])

##5 plotting the World Bank regions 
 palette<-brewer.pal(8,"Dark2")
 plot(emissions_death_pop_DF$death_by_population_DF.Deaths_by_million, emissions_death_pop_DF$population_dens.X2018.pop_ID., cex=0, ylab="Emissions", xlab="Cumulated death per million as of 3rd Oct 2020", ylim=c(0,700))
 text(emissions_death_pop_DF$death_by_population_DF.Deaths_by_million, emissions_death_pop_DF$population_dens.X2018.pop_ID., death_pop_DF$death_by_population_DF.Group.1, cex=0.5, col=palette[as.factor(emissions_death_pop_metadata_DF$metadata.World.bank.region.metadata_ID.)])
 legend("topright", levels(as.factor(emissions_death_pop_metadata_DF$metadata.World.bank.region.metadata_ID.)), col=palette, bty="n", pch=16, cex=0.8)
 
 
##6 Reduce the plot to Europe & Central Asia
 png("Assignment_3_plot.png", width = 2000, height = 2000, res=300)
 select_europe<-emissions_death_pop_metadata_DF$metadata.World.bank.region.metadata_ID.=="Europe & Central Asia"
 income_vec=as.factor(emissions_death_pop_metadata_DF$metadata.World.bank..4.income.groups.2017.metadata_ID.[select_europe])
 palette_seq<-brewer.pal(3, "Set1")
 plot(emissions_death_pop_DF$death_by_population_DF.Deaths_by_million[select_europe], emissions_death_pop_DF$population_dens.X2018.pop_ID.[select_europe], cex=emissions_death_pop_DF$emissions.X2018.emissions_ID.[select_europe]*0.6, 
      ylab="Current population density per square km", xlab="Cumulated death per million", ylim=c(0,670), xlim=c(0, 1300), col=add.alpha(palette_seq[income_vec], 0.2), pch=16)
 text(emissions_death_pop_DF$death_by_population_DF.Deaths_by_million[select_europe], emissions_death_pop_DF$population_dens.X2018.pop_ID.[select_europe], death_pop_DF$death_by_population_DF.Group.1[select_europe], cex=0.6, col=palette_seq[income_vec])
 legend("bottomright", levels(income_vec), text.col=palette_seq, bty="n", cex=0.8)
 legend("topleft", "Size corresponds to emissions (t per person per year)", col = add.alpha(palette_seq, 0.25)[1],  pt.bg = add.alpha(palette_seq, 0.25)[1], bty="n", pch=21, pt.cex=2.5, cex=0.8)
 dev.off()
 