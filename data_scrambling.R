
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
population_scrambled<-population
for(i in 2:dim(population_scrambled)[2]){
  population_scrambled[as.logical(rbinom(dim(population_scrambled)[1], 10, 0.0015)), i]="Error: missing"
}

####### ADDING WHITE SPACES
deaths_scrambled<-deaths
for(i in 2:dim(deaths)[2]){
  deaths_scrambled[as.logical(rbinom(dim(deaths_scrambled)[1], 10, 0.002)), i]=" "
}


colnames(metadata)[3]<-"four regions"
colnames(metadata)[4]<-"eight regions"
colnames(metadata)[6]<-"six regions"






####### MESSING UP SOME SEPARATORS IN THE .csv
write.table(emissions, "./corrupted_data/co2_emissions_tonnes_per_person.csv", sep=";", dec = ",")
write.table(population_dens_scrambled, "./corrupted_data/population_density_per_square_km.csv", sep="\t")
write.table(population_scrambled, "./corrupted_data/population_total.csv")
write.table(deaths_scrambled, "./corrupted_data/time_series_covid19_deaths_global.csv")
write.table(metadata, "./corrupted_data/Countries metadata.csv")



