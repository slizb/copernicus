
library(copernicus)
library(animation)

inits <- initialize_system('earth' = earth(), 
                           'sun' = sun() )

constants <- set_constants()

# run simulation -----------------------------------------------------

simulate_system <- function(inits, constants, n_steps, file_out) {
  saveGIF(interval = .05,
          movie.name = file_out,
          expr = {
            
            bodies <- inits
            df <- data.frame()
            for (i in 1:n_steps) {
              # inner loop to iterate over each body
              for (body_a in names(bodies)) {
                # another inner loop to compute force exerted by each other body
                for (body_b in names(bodies)) {
                  # skip force of a body on itself
                  if (body_a == body_b) { next }
                  
                  # look up state, pass to leapfrog
                  # compute distance between 2 bodies
                  x <- bodies[[body_a]]$position[1] - bodies[[body_b]]$position[1]
                  y <- bodies[[body_a]]$position[2] - bodies[[body_b]]$position[2]
                  vx <- bodies[[body_a]]$velocity[1] 
                  vy <- bodies[[body_a]]$velocity[2]
                  m <- bodies[[body_b]]$mass
                  
                  next_sequence <- execute_leapfrog(x, y, vx, vy, constants, m)
                  
                  # update state
                  bodies[[body_a]]$position[1] <- next_sequence[[1]]
                  bodies[[body_a]]$position[2] <- next_sequence[[2]]
                  bodies[[body_a]]$velocity[1] <- next_sequence[[3]]
                  bodies[[body_a]]$velocity[2] <- next_sequence[[4]]
                  
                }
                record <- data.frame(step = i, 
                                     body = body_a, 
                                     x = bodies[[body_a]]$position[1],
                                     y = bodies[[body_a]]$position[2])
                
                df <- rbind(df, record)
                print(body_a)
                print(i)
                print(record)
                
              }
              current_state <- dplyr::filter(df, step == i)
              print(plot_system(current_state)) 
            }
          }
  )
  return(df)
  
}

simulation <- simulate_system(inits, constants, 365, 'hodor.gif')







