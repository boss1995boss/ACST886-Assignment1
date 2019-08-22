#Assignment1-Q4

#a
bir_A = as.Date("1965-05-16") ;bir_B = as.Date("1922-03-01") ; bir_C = as.Date("1942-09-21");
pa_A = as.Date("1985-07-21") ;pa_B = as.Date("1955-09-23") ; pa_C = as.Date("1967-06-02");
death_A = as.Date("1994-06-22") ;death_B = as.Date("1993-07-21") ; death_C = as.Date("1996-04-29");
birth = c(bir_A, bir_B, bir_C); death = c(death_A, death_B, death_C); pa = c(pa_A, pa_B, pa_C)
age_a = function(birth, death){
  age_a = floor(as.numeric(death-birth)/365.25)+1
}
print(age_a(birth, death))

#b
age_b = function(birth, death){
  year = as.character(substring(death,1,4))
  starting_day = "01-01"
  day = c()
  for(i in 1:length(death)){day[i] = (paste(c(year[i], starting_day), collapse = "-"))}
  day = as.Date(day)
  age_b = floor(as.numeric(day-birth)/365.25)
}
print(age_b(birth, death))

#c
age_c = function(birth, death, pa){
  temp = c()
  year = as.character(substring(death,1,4))
  starting_day = substring(birth,6,10)
  starting_day_pol = substring(pa, 6,10)
  day = c()
  pol = c()
  for(i in 1:length(death)){
    day[i] = (paste(c(year[i], starting_day[i]), collapse = "-"))
    pol[i] = (paste(c(year[i], starting_day_pol[i]), collapse = "-"))
  }
  day = as.Date(day)
  pol = as.Date(pol)
  for(i in 1:length(death)){
    if(as.numeric(day[i]-death[i])<0 & as.numeric(death[i]-pol[i])<0)
      temp[i] = floor((as.numeric(day[i]-birth[i])+1)/365.25)
    else
      temp[i] = floor(as.numeric(day[i]-birth[i])/365.25)-1
  }
  age_c = temp
}
print(age_c(birth, death, pa))
