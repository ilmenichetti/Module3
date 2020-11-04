
# this file is meant just to build up the basic analysis on the data we want the students to perform
# I will then corrupt the datafile

## Data read
liberty_index<-read.csv("cliberities_fh.csv")
emissions<-read.csv("co2_emissions_tonnes_per_person.csv")
population_dens<-read.csv("population_density_per_square_km.csv")
population<-read.csv("population_total.csv")
deaths<-read.csv("time_series_covid19_deaths_global.csv")
cases<-read.csv("time_series_covid19_confirmed_global.csv")


##0 collapse the Covid data to have 1 nation only and rescale deaths by pop size
  # 0.0 collapse data
  deaths_agg<-aggregate(deaths[5:dim(deaths)[2]], list(deaths$Country.Region), sum)
  #0.1 rescale deaths by population
  population_ID<-match(deaths_agg$Group.1,population$country)
  #0.2 build a combined data frame
  # I put both ID columns to doublecheck the matching is going fine 
  death_population_DF<-data.frame(deaths_agg$Group.1,population$country[population_ID], deaths_agg$X11.3.20, population$X2018[population_ID])
  #0.3 deaths normalized per million people
  death_by_population_DF<-data.frame(death_population_DF$deaths_agg.Group.1, death_population_DF$deaths_agg.X11.3.20/(death_population_DF$population.X2018.population_ID.*10^-6)) 
  colnames(death_by_population_DF)<-c("Group.1", "Deaths_by_million")

##1  Do we have an effect of liberty on total deaths?
  # 1.1 find which nations in the death table are matching in the liberty index table and relative indexes
  liberty_ID<-match(death_by_population_DF$Group.1,liberty_index$country)
  #1.2 build a combined data frame
  # I put both ID columns to doublecheck the matching is going fine 
  death_liberty_DF<-data.frame(death_by_population_DF$Group.1,liberty_index$country[liberty_ID], death_by_population_DF$Deaths_by_million, liberty_index$X2018[liberty_ID])
  #1.3 plot
  plot(death_liberty_DF$death_by_population_DF.Deaths_by_million, death_liberty_DF$liberty_index.X2018.liberty_ID., cex=0, ylab="Liberty Index", xlab="Cumulated death per million as of 3rd Oct 2020")
  text(death_liberty_DF$death_by_population_DF.Deaths_by_million, death_liberty_DF$liberty_index.X2018.liberty_ID., death_liberty_DF$death_by_population_DF.Group.1, cex=0.5)

##2  Do we have an effect of populationd density on total deaths? And emissions?
  # 2.1 find which nations in the death table are matching in the liberty index table and relative indexes
  pop_ID<-match(death_by_population_DF$Group.1,population_dens$country)
  #2.2 build a combined data frame
  # I put both ID columns to doublecheck the matching is going fine 
  death_pop_DF<-data.frame(death_by_population_DF$Group.1,population_dens$country[pop_ID], death_by_population_DF$Deaths_by_million, population_dens$X2018[pop_ID])
  #2.3 plot
  plot(death_pop_DF$death_by_population_DF.Deaths_by_million, death_pop_DF$population_dens.X2018.pop_ID., cex=0, ylab="Density", xlab="Cumulated death per million as of 3rd Oct 2020")
  text(death_pop_DF$death_by_population_DF.Deaths_by_million, death_pop_DF$population_dens.X2018.pop_ID., death_pop_DF$death_by_population_DF.Group.1, cex=0.5)
  # Ok, Monaco is quite off the scale...
  plot(death_pop_DF$death_by_population_DF.Deaths_by_million, death_pop_DF$population_dens.X2018.pop_ID., cex=0, ylab="Density", xlab="Cumulated death per million as of 3rd Oct 2020", ylim=c(0,2500))
  text(death_pop_DF$death_by_population_DF.Deaths_by_million, death_pop_DF$population_dens.X2018.pop_ID., death_pop_DF$death_by_population_DF.Group.1, cex=0.5)
  
##3 emissions and density
 emissions_ID<-match(death_pop_DF$death_by_population_DF.Group.1,emissions$country)
 emissions_death_pop_DF<-data.frame(death_pop_DF, emissions$country[emissions_ID], emissions$X2018[emissions_ID])
 
 plot(emissions_death_pop_DF$death_by_population_DF.Deaths_by_million, emissions_death_pop_DF$emissions.X2018.emissions_ID., cex=0, ylab="Emissions", xlab="Cumulated death per million as of 3rd Oct 2020", ylim=c(0,40))
 text(emissions_death_pop_DF$death_by_population_DF.Deaths_by_million, emissions_death_pop_DF$emissions.X2018.emissions_ID., death_pop_DF$death_by_population_DF.Group.1, cex=0.5)
 
