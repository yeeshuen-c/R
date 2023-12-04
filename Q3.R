# Function to calculate Euclidean distance between two points
euclidean_distance <- function(p1, p2) {
  sqrt((p2[1] - p1[1])^2 + (p2[2] - p1[2])^2)
}

# Function to find the nearest neighbor for a given point
find_nearest_neighbor <- function(point, unvisited, distances) {
  min_dist <- Inf
  nearest <- NULL
  
  for (i in unvisited) {
    if (i != point && distances[point, i] < min_dist) {
      min_dist <- distances[point, i]
      nearest <- i
    }
  }
  
  return(nearest)
}

# Function to generate a Hamiltonian tour using Sorted Edges algorithm
sorted_edges_hamiltonian_tour <- function(points) {
  num_points <- nrow(points)
  distances <- matrix(0, nrow = num_points, ncol = num_points)
  
  # Calculate distances between points
  for (i in 1:num_points) {
    for (j in 1:num_points) {
      distances[i, j] <- euclidean_distance(points[i,], points[j,])
    }
  }
  
  # Initialize variables
  unvisited <- 1:num_points
  tour <- c(1)  # Start with the first point
  unvisited <- unvisited[-1]  # Remove the first point from unvisited
  
  while (length(unvisited) > 0) {
    min_cost <- Inf
    selected_edge <- NULL
    
    for (i in tour) {
      nearest_neighbor <- find_nearest_neighbor(i, unvisited, distances)
      
      if (!is.null(nearest_neighbor)) {
        cost <- distances[i, nearest_neighbor]
        
        if (cost < min_cost && !(length(tour) == num_points - 1 && sum(colSums(distances[tour,])) == 3)) {
          min_cost <- cost
          selected_edge <- c(i, nearest_neighbor)
        }
      }
    }
    
    if (!is.null(selected_edge)) {
      tour <- c(tour, selected_edge[2])
      unvisited <- unvisited[unvisited != selected_edge[2]]
    } else {
      break
    }
  }
  
  # Complete the tour
  tour <- c(tour, tour[1])
  
  # Calculate the total distance of the round trip
  total_distance <- sum(distances[tour[-length(tour)], tour[-1]])
  
  return(list(tour = tour, total_distance = total_distance))
}

# Generating random points for demonstration (10 points)
set.seed(123)
points <- matrix(
  c(60, 200,
    180, 200,
    80, 180,
    140, 180,
    20, 160,
    100, 160,
    200, 160,
    140, 140,
    40, 120,
    100, 120),
  ncol = 2,
  byrow = TRUE
)
tour_result <- sorted_edges_hamiltonian_tour(points)
cat("Hamiltonian Tour:", tour_result$tour, "\n")
cat("Total Distance:", tour_result$total_distance, "\n")
