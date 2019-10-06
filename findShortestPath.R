library(readxl)
library(data.table)

station_transfer = function(subway){
  # all station (departure station and arrival station)
  station <- subway[,.(All_station = c(Station_start, Station_end[length(Station_end)])),by=Line]
  
  # find transfer station
  transfer <- station[,.(times = length(Line)),by=All_station][times > 1, All_station]
  
  # rename transfer station
  station[, Rename := All_station]
  station[station$All_station %in% transfer, Rename := paste(Line, All_station, sep = '-')]
  
  return(list(station=station,transfer=transfer))
}

dist_matrix = function(subway,station,transfer,transferTime){
  # all station distance matrix
  station_num <- length(station$Rename)
  distance_matrix <-  matrix(10000000, nrow = station_num, ncol = station_num)
  diag(distance_matrix) <- 0   # diag = 0
  
  line_unique <- unique(subway$Line)
  start_index <- 0
  ## fill each small matrix by Line, upper triangular matirx
  for(i in 1:length(line_unique)){
    time_by_line <- subway[Line ==line_unique[i], Time]
    n_time <- length(time_by_line)
    diag(distance_matrix[(start_index+1):(start_index+n_time), 
                     (start_index+2):(start_index+n_time+1)]) <- time_by_line
    start_index <- start_index + n_time + 1
  }
  
  # add transfer station distance
  transfer_dist <- transferTime  # set transfer time
  transfer_index <- sapply(transfer, function(x) which(x == station$All_station))
  sapply(transfer_index, function(x) distance_matrix[x[1], x[2]] <<- transfer_dist)
  
  # circle station
  which(subway == "2-西直门")
  which(subway == "2-车公庄")
  which(subway == "巴沟")
  which(subway == "火器营")
  distance_matrix[24, 41] <- 2
  distance_matrix[199, 243] <- 3
  
  # upper triangular to symmetry
  distance_matrix[lower.tri(distance_matrix)] <- t(distance_matrix)[lower.tri(distance_matrix)]
  return(distance_matrix)
}

findShortestPath_prepare = function(location, destination, transfer, distance_matrix,station)
  {
  # test
  Rcpp::sourceCpp("shortestpath.cpp")
  output = function(I_finally, start, end){
    I_finally[1,] <- I_finally[1, ] + 1    # change C++ index to R index
    last_visit <- end
    path <- c(last_visit)
    dist <- I_finally[2, last_visit]
    while (last_visit != start) {
      last_visit <- I_finally[1, last_visit]
      path <- c(last_visit, path)
    }
    return(list(path = path, distance = dist))
  }
  
  W = distance_matrix
  
  n <-  nrow(W)
  subway <- station$Rename
  
  # judge whether location or destination is transfer station
  if(location %in% transfer)
    location <- station[All_station == location, Rename][1]
  if(destination %in% transfer)
    destination <- station[All_station == destination, Rename][1]
  start_num <- which(subway == location)
  end_num <- which(subway == destination)
  
  # Make sure local and destination are in the subway.
  if(length(start_num) > 0 & length(end_num) > 0){
    I_res <- computePath(weight = W, start = start_num)
    path_res <- output(I_finally = I_res, start = start_num, end = end_num)
    cat("The shortest path from", location, "to", destination, "is:\n", 
        paste(subway[path_res$path], collapse = ' --> '), 
        "\nThe total distance is:\n",
        path_res$distance)
  }else{
    print("error input")
  }
}

findShortestPath <- function(location, destination, transferTime)
{
  subway <- read_xlsx("merged.xlsx")
  subway <- as.data.table(subway)
  colnames(subway) <- c("Line","Station_start","Station_end","Time")
  
  station_transfer_name <- station_transfer(subway)
  station <- station_transfer_name$station
  transfer <- station_transfer_name$transfer
  dist <-  dist_matrix(subway,station,transfer,transferTime) 
  findShortestPath_prepare(location, destination, transfer, dist,station)
}


location <- "苹果园"
destination <-  "五道口"
transferTime <- 7

findShortestPath(location, destination, transferTime)