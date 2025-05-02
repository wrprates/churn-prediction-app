# plumber.R
library(plumber)

#* @get /hello
function(){
  list(msg = "Hello World!")
}
