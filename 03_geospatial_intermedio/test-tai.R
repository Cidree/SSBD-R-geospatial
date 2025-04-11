
precipStack = prec_sr
PETstack    = pet_sr
precipScale <- 1





#create receiving rasters
d <- precipStack[[1]]
terra::values(d)[!is.na(terra::values(d))] <- 0
n <- precipStack[[1]]
terra::values(n)[!is.na(terra::values(n))] <- 0

# for each month, determine which cells have precip < PET
# for those cells, add value of diff to receiver rasters
# for those cells, add PET to n
for (i in 1:12) {
  
  isPrecipLess <- precipStack[[i]] < PETstack[[i]]
  cells <- which(terra::values(isPrecipLess) == 0)
  tmp <- PETstack[[i]] - precipStack[[i]]
  tmp[cells] <- 0
  d <- d + tmp

  tmp <- PETstack[[i]]
  terra::values(tmp)[cells] <- 0
  n <- n + tmp
}

res <- 100*d
res <- res / n
nzeroes <- which(terra::values(n) == 0)
res[nzeroes] <- 0
names(res) <- 'aridityIndexThornthwaite'
return(res)



## CHAT GPT

# Assume: 12 layers each (monthly)
precip <- prec_sr   # SpatRaster with 12 layers
pet <- pet_sr         # SpatRaster with 12 layers

# Identify where precipitation < PET (for each month)
deficit_months <- precip < pet

# Compute monthly deficits (PET - P), but only where P < PET
monthly_deficit <- mask(pet - precip, deficit_months)

# Sum of deficits across months
d_sum <- app(monthly_deficit, sum, na.rm = TRUE)

# Sum of PET across months where P < PET
masked_pet <- mask(pet, deficit_months)
n_sum <- app(masked_pet, sum, na.rm = TRUE)

# Thornthwaite Aridity Index
ai <- 100 * d_sum / n_sum
