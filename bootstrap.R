#bootstrap

model <-  lm(`l/100km` ~  displacement +  horsepower + weight + acceleration + year + cyl_6 + cyl_8 +JP+EU,
             data = cars)
summary(model)



boot_cars <- cars


par_boot_cars <- data.frame(const=integer(),
                            displacement=integer(),
                            horsepower=integer(),
                            weight=integer(),
                            acceleration=integer(),
                            year=integer(),
                            cyl_6=integer(),
                            cyl_8=integer(),
                            JP=integer(),
                            EU=integer())


for (i in 1:length(par_boot_cars)) {

  for (n in 1:10^3) {
  boot_sample <- sample(1:nrow(cars), 
                        nrow(cars), 
                        replace = T)
  model <- lm(`l/100km` ~  displacement +  horsepower + weight + acceleration + year + cyl_6 + cyl_8 +JP+EU,
              data = boot_cars[boot_sample,])
  par_boot_cars[n,i] <- coef(model)[i]
  
  }
  
}

for (i in 1:ncol(par_boot_cars)) {
  
  if(prod(quantile(par_boot_cars[,i], c(0.025, 0.975)))<0){
    par_boot_cars[,i] <- 0
  }
  else{
    
  }
  
}

par <- data.frame(const=integer(),
                  displacement=integer(),
                  horsepower=integer(),
                  weight=integer(),
                  acceleration=integer(),
                  year=integer(),
                  cyl_6=integer(),
                  cyl_8=integer(),
                  JP=integer(),
                  EU=integer())

for (i in 1:ncol(par_boot_cars)) {
  
  par[1,i] <- mean(par_boot_cars[,i])
  
}

par


par_boot_cars <- data.frame(const=integer(),
                            horsepower=integer(),
                            weight=integer(),
                            year=integer(),
                            cyl_6=integer(),
                            JP=integer()
                           
                            
                            )



for (i in 1:length(par_boot_cars)) {
  
  for (n in 1:10^3) {
    boot_sample <- sample(1:nrow(cars), 
                          nrow(cars), 
                          replace = T)
    model <- lm(`l/100km` ~   horsepower + weight + year + cyl_6 + JP  ,
                data = boot_cars[boot_sample,])
    par_boot_cars[n,i] <- coef(model)[i]
    
  }
  
  
  
  
}

for (i in 1:ncol(par_boot_cars)) {
  
  if(prod(quantile(par_boot_cars[,i], c(0.025, 0.975)))<0){
    par_boot_cars[,i] <- 0
  }
  else{
    
  }
  
}

par <- data.frame(const=integer(),
                  horsepower=integer(),
                  weight=integer(),
                  year=integer(),
                  cyl_6=integer(),
                  JP=integer())

for (i in 1:ncol(par_boot_cars)) {
  
  par[1,i] <- mean(par_boot_cars[,i])
  
}

par


boot_model <- lm(`l/100km` ~  horsepower + weight + year + cyl_6, data = cars)
summary(boot_model)
#95%
xd <- quantile(par_boot_cars[,8], c(0.025, 0.975))
prod(xd)<0

