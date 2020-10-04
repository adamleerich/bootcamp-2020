source('common.R')

## "This is the name of someone"
## 'This is also the name of someone'
## "This is someone's name"
## 'This is also someone''s name'

people <- c('Brian', 'Adam', 'Kellee', 'Leanne')
characteristics <- c('cranky', 'jolly', 'jolly', 'jolly')
sort(people)
sort(characteristics)

order(people)
people[order(people)]
sort(people)

which(people == 'Adam')

happy_people <- which(characteristics != 'cranky')
people[happy_people]

table(characteristics)

add_two <- function(thing_one, thing_two){
  thing_one + thing_two
}

add_two(3, 4)

for (i in seq_len(5)) {
  print(i)
}

for (i_name in seq_along(people)) {
  print(people[i_name])
}

count_e <- function(x) {
  str_count(x, '[Ee]')
}

lapply(people, count_e)

map(people, count_e)
map_chr(people, count_e)

for (i_name in seq_along(people)) {
  people[i] <- tolower(people[i])
}

people <- toupper(people)

## if (my_value == something) {
##   print("My value equals something")
## }
## 
## if (my_value == something) {
##   print("My value equals something")
## } else if (my_value == something_else) {
##   print("My value equals something else")
## } else ()

if (people == "Adam") {
  print("Hi Adam!")
}

ifelse(
  people == "Adam"
  , 'In Connecticut'
  , 'Somewhere else'
)
