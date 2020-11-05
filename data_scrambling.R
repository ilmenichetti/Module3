
## Data read
#liberty_index<-read.csv("cliberities_fh.csv")
emissions<-read.csv("co2_emissions_tonnes_per_person.csv")
population_dens<-read.csv("population_density_per_square_km.csv")
population<-read.csv("population_total.csv")
deaths<-read.csv("time_series_covid19_deaths_global.csv")
metadata<-read.csv("Countries_metadata.csv")


### ADDING NAS
population_dens_scrambled<-population_dens
for(i in 2:dim(population_dens)[2]){
  population_dens_scrambled[as.logical(rbinom(dim(population_dens_scrambled)[1], 10, 0.005)), i]=NA
}
