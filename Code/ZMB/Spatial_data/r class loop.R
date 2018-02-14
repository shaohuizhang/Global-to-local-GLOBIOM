

function(df){
  i <- 1
  ranked <- F
  repeat{if(ranked) break}{
    eval[i,]
    if(all(val %in% eval)) {
      rank <- val
      ranked <- T
    } else{
      i = i + 1
      if(i == max+1){}
        print("no rank")
        stop
    }
  }
  return(rank)
}