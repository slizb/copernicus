
library(animation)
library(magrittr)
library(ggplot2)

rm(list=ls())

# constants ----------------------------------------------------------

G = 6.67408e-11     # Newton's gravitational constant (m3 kg-1 s-2)
M = 1.989e30        # mass of the sun (kg)
dt = 8.64e4         # time step -one day (seconds)


# initializations ----------------------------------------------------

x = 1.496e11             # (m)
y = 0                    # (m)
vx_prev_half = 0         # (m/s)
vy_prev_half = 2.978e4   # (m/s)


# functions ----------------------------------------------------------


# leapfrog algorithm -------------------------------------------------

saveGIF(interval = .05,
        expr = {
             
             for (i in 1:365){
                  vx_next_half = vx_prev_half - dt * G * M * x * (x^2 + y^2) ^ (-3/2)
                  x_next = x + dt * vx_next_half
               
                  vy_next_half = vy_prev_half - dt * G * M * y * (x^2 + y^2) ^ (-3/2)
                  y_next = y + dt * vy_next_half
                  
                  # plot
                  df <- data.frame(rbind(c(x=x, y=y), c(x=0,y=0)))
                  gg <- ggplot(df, aes(x=x, y=y))
                  points <- geom_point()
                  x_lim <- xlim(-2e11, 2e11) 
                  y_lim <- ylim(-2e11, 2e11)
                       
                  print(gg + points + x_lim + y_lim)
                  
                  # update positions
                  x = x_next
                  y = y_next
                  
                  # update velocities
                  vx_prev_half = vx_next_half
                  vy_prev_half = vy_next_half
                  
             }
        }
)

