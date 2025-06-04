treatment <- function(stage, d, id){
  #set.seed(id+3000000)
  b=0
  s = stage
  r = round
  #treatment burden
  #no treatment; chemo only or radiation only or other treatment; surgery only; chemo and radiation
  #surgery and chemo or surgery and radiation; surgery + chemo + radiation
  burden = trtburden[d,]
  prob = runif(1,0,1)
  if(prob<=trt_r0[s, 1]){b = b+burden[1]}
  else if(prob<=sum(trt_r0[s, 1:2])){b = b+burden[2]}
  else if(prob<=sum(trt_r0[s, 1:3])){b = b+burden[3]}
  else if(prob<=sum(trt_r0[s, 1:4])){b = b+burden[4]}
  else if(prob<=sum(trt_r0[s, 1:5])){b = b+burden[5]}
  else{b = b+burden[6]}
  return(b)
}
