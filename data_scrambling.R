
## Data read
emissions<-read.csv("co2_emissions_tonnes_per_person.csv")
population_dens<-read.csv("population_density_per_square_km.csv")
population<-read.csv("population_total.csv")
deaths<-read.csv("time_series_covid19_deaths_global.csv")
metadata<-read.csv("Countries_metadata.csv")


####### ADDING NAS
population_dens_scrambled<-population_dens
for(i in 2:dim(population_dens)[2]){
  population_dens_scrambled[as.logical(rbinom(dim(population_dens_scrambled)[1], 10, 0.005)), i]=NA
}
####### ADDING -9999
for(i in 2:dim(population_dens)[2]){
  population_dens_scrambled[as.logical(rbinom(dim(population_dens_scrambled)[1], 10, 0.005)), i]=-9999
}

####### MIXING A FEW CHARACTERS IN SOME COLUMNS TO MAKE IT READ LIKE FACTOR
for(i in 2:dim(population)[2]){
  population[as.logical(rbinom(dim(population)[1], 10, 0.0015)), i]="Error: missing"
}

####### ADDING WHITE SPACES
for(i in 2:dim(deaths)[2]){
  deaths[as.logical(rbinom(dim(deaths)[1], 10, 0.002)), i]=" "
}



####### MESSING UP SOME SEPARATORS IN THE .csv
write.csv(emissions, "./corrupted_data/co2_emissions_tonnes_per_person.csv")
write.csv(population_dens, "./corrupted_data/population_density_per_square_km.csv")
write.csv(population, "./corrupted_data/population_total.csv")
write.csv(deaths, "./corrupted_data/time_series_covid19_deaths_global.csv")
write.csv(metadata, "./corrupted_data/Countries_metadata.csv")



