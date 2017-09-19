

set_constants <- function(G = 6.67408e-11,
                          M = 1.989e30,
                          dt = 8.64e4,
                          au = 1.496e11) {
  constants <- list("G" = G,
                    "M" = M,
                    "dt" = dt,
                    "au" = au)
  return(constants)
  
}




# initializations ----------------------------------------------------

x <- 1.496e11             # (m)
y <- 0                    # (m)
vx_prev_half <- 0         # (m/s)
vy_prev_half <- 2.978e4   # (m/s)

# rescale to au's ----------------------------------------------------

G <- G / au^3
x <- x / au
y <- y / au
vx_prev_half <- vx_prev_half / au
vy_prev_half <- vy_prev_half / au
