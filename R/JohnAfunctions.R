#' A function to quickly retrieve my current Steam history
#'
#' This function returns the tabs of my Steam library:
#' My most played games, my recently played games, and my games with a perfect score.
#' @param ... There are no parameters for this function.
#' @keywords PiPS Steam Q3.2.1
#' @export
#' @examples 
#' remind_me()

remind_me <- function() {
  top_games <- c("Crusader Kings 2", "Binding of Isaac Rebirth", "The Elder Scrolls: Skyrim", "Stardew Valley", "Binding of Isaac", "Slay the Spire", "Age of Mythology: Extended Edition")
  recent_games <- c("Stardew Valley", "while True: Learn()", "Inscryption", "The Jackbox Party Pack 3", "The Jackbox Party Pack 4")
  perfect_games <- matrix(c("Manifold Garden", "Poker Night 2", "Return of the Obra Dinn", "The Binding of Isaac", "The Room Two", 25, 18, 16, 99, 7), 
                          2, 
                          5, 
                          byrow = TRUE,
                          dimnames = list(c("Game", "# of Achievements"),
                                          c(1:5)))
  
  steam_games <- list("most_played" = top_games, "recent" = recent_games, "perfect" = perfect_games)
  return(steam_games)
}
remind_me()

#' Giving you my answer for an exercise from 3.1
#'
#' This function returns my answer for the first 10 questions from the 3.1 exercise sheet.
#' 
#' @param exercise_number Requires to be a number between 1 to 10.
#' @keywords PiPS Exercises Q3.1 Q3.2.2 
#' @export
#' @examples
#' cheat(1)
#' # Q3.1.1
#'
#' my_sample <- rnorm(1000)
#'
#' boxplot(my_sample))

cheat <- function(exercise_number) {
  if(!exercise_number %in% c(1:10)) stop("Please type a number between 1 and 10")
  else {
   switch(exercise_number,
          cat('# Q3.1.1
              \nmy_sample <- rnorm(1000)
              \nboxplot(my_sample))'),
          cat('# Q3.1.2
              \n# Load in the dataset
              \nschiphol_temp <- read.csv("https://raw.githubusercontent.com/hannesrosenbusch/schiphol_class/master/schiphol_data.csv")
              \n# Then plot the graph
              \nplot(schiphol_temp$DATE, schiphol_temp$TAVG)'),
          cat('# Q3.1.3
              \nlibrary("ggplot2")
              \n# Load in the data
              \ntitanic_data <- read.csv("https://raw.githubusercontent.com/hannesrosenbusch/schiphol_class/master/titanic.csv")
              \n
              \n# Change the values of $Survived with 0 becoming dead and 1 becoming alive
              \ntitanic_data$Survived <- factor(titanic_data$Survived, levels = c(0,1), labels = c("dead", "alive"))
              \n # Then plot the graph with the label being changed
              \n ggplot(titanic_data) +
              \n        geom_bar( mapping = aes( x = Sex,
              \n                                 fill = Survived)
              \n                                ) +
              \n        labs(fill = "How did it go?")'),
          cat('# Q3.1.4
              \nggplot(titanic_data) +
              \n     geom_bar( mapping = aes( x = Sex,
              \n                              fill = Survived)
              \n                            ) +
              \n     labs(fill = "How did it go?") +
              \n     ggthemes::theme_tufte()
              \n# This theme removes the unnecessary axes'),
          cat('# Q3.1.5
              \n# Original plot
              \nplot(mtcars$cyl, mtcars$hp)
              \nggplot(mtcars) +
              \n# Change the graph to have sharper points that are easier to read
              \n        geom_point( mapping = aes( x = cyl,
              \n                                   y = hp
              \n                                  )
              \n                  ) +
              \n# Adding labels: Title, x-axis, y-axis
              \n        labs( title = "Horsepower of cars by number of cylinders",
              \n              x = "N umber of Cylinders",
              \n              y = "Horsepower"
              \n            ) +
              \n 
              \n# Making the y-axis logarithmic to more easily see the individual points
              \n        scale_y_continuous( trans = "log10") +
              \n  
              \n# Adding a theme that looks more crisp
              \n        theme_bw()'),
          cat('# Q3.1.6
              # Just change the order manually. 
              \nOrange$Tree <- factor(Orange$Tree, levels = c(1:5))
              \n
              \n# Since ggplots aes x and y values need to match, I either had to reduce the existing dataframe
              \n# by deleting values (which is usually a no-go) or I needed to blow up max_circumference to a 35-element vector.
              \nmax_circumference <- numeric(35)
              \n# Write a simple for loop that fills in the max value at every 7th position. 
              \n# This circumvents geom_col to change values by adding up all elements per subgroup. 
              \nfor (i in 1:5) {
              \n  max_circumference[i * 7] <- max(subset(Orange, Orange$Tree == i)[,3])
              \n}
              \n# Now we just plot the graph et voila!
              \nggplot(Orange) +
              \n       aes( Tree, max_circumference) +
              \n       geom_col()'), 
          cat('# Q3.1.7
              \nggplot(Orange) +
              \naes( age, circumference) +
              \ngeom_smooth( method = glm)'),
          cat('# Q3.1.8
              \n# Copying the code from Q3.1.6 and assigning it to an object 
              \ng1 <- ggplot(Orange) +
              \n             aes( x = Tree,
              \n                  y = max_circumference) +
              \n             geom_col()
              \n# Now create the line graph on the right side
              \ng2 <- ggplot(Orange) +
              \n# As it turns out, it is geom_path that was used for this graph.
              \n             geom_path( mapping = aes( age,
              \n                                       circumference,
              \n                                       color = Tree), # Color by Tree, thank God there is no 0 this time...
              \n                        group = Orange$Tree) # Sort by Tree as well for 5 graphs
              \ngridExtra::grid.arrange(g1, g2, ncol = 2) # Neat little package that does something similar to patchwork'),
          cat('# Q3.1.9
              \nlibrary(ggstatsplot)
              \nggbetweenstats(ToothGrowth, supp, len)
              \n# As we can see, the t-test is barely inconclusive, the Bayes Factor does show a moderate effect
              \n# of bigger teeth when guinea pigs drink orange juice compared to the supplement group.'), 
          cat('# Q3.1.10
              \nlibrary(plotly)
              \n# Adapted this link: https://plotly.com/r/3d-scatter-plots/
              \nfig <- plot_ly(iris, x = ~Petal.Width, y = ~Petal.Length, z = ~Sepal.Width, color = ~Species)
              \nfig <- fig %>% add_markers()
              \nfig <- fig %>% layout()
              \nfig')
          )
  }
}