#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'	Initial Door Selection
#' @description
#'	'select_door' creates a function that chooses the initial pick of the contestant
#' @details
#'	This function replicates the contestant initially picking a door behind which they believe is a car. 
#' @param 
#'	No arguments are used by the function.
#' @return 
#'	The function returns one number corresponding to a door, which will either be 1, 2, or 3. 
#' @examples
#'	select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'	Open a Door with a Goat Behind It
#' @description
#'	This function opens a door that both has a goat behind it and is not the door the contestant initially picked.
#' @details
#'	This function uses the information from the previous two functions to eliminate one of the doors the contestant can choose.
#'	It will open a door in which there is a goat behind it, but it will not be the door the contestant initially picked. 
#' @param 
#'	This function takes in two inputs: game, which will be linked to create_game(), and a.pick() which will be linked to select_door(). 
#' @return 
#'	This function will return a number, either 1, 2, or 3. It will not be the same number as from select_door().
#' @examples
#'	open_goat_door(game, a.pick)
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'	Switch or Stay
#' @description
#'	This function will either let the contestant either stay with their initial pick, or switched to the other unopened door.
#' @details
#'	This function takes on three inputs, which will allow the contestant to stick with their initial door, or change their decision to the
#'	as of yet unopened door. The output will be the final decision of the contestant as to which door has the car behind it. 
#' @param 
#'	This functions takes on three inputs: whether the contestant stays with their initial pick (stay=T) or switches (stay=F), which door was
#'	opened in the previous step, and which door was the contestant's initial pick. 
#' @return 
#'	The function will return either 1, 2, or 3 corresponding to the contestant's final pick
#' @examples
#'	change_door(stay=T, opened.door, a.pick )
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#'	Determine Outcome
#' @description
#'	This function reveals if the contestant won or lost the game.
#' @details
#'	The function uses two inputs to determine whether the final pick is associated with the car or not. If it is, the function
#'	declares them the winner (WIN), and if not, declares that they have lost (LOSE). 
#' @param 
#'	The function uses two inputs, final.pick, which is the contestant's final decision on door, and game, from the create_game()function
#'	that will allow it to determine which door has a car behind it. 
#' @return 
#'	It will return either WIN or LOSE.
#' @examples
#'	determine_winner(final.pick, game)
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'	Play the Monty Hall Problem
#' @description
#'	This executes all the functions of the Monty Hall package in the correct order to play one game.
#' @details
#'	The code assigns functions to specific objects to allow for function input in the functions that require it. The code
#`	first creates a new game, picks a door, opens a goat door, and then splits into a switch and a stay decision, showing
#'	the result of both strategies in the game. It then prints this information in an easily interpretable data frame. 
#' @param 
#'	This function takes no inputs. 
#' @return 
#'	The function returns a 2x2 data frame, with each strategy getting a WIN or LOSE judgement. 
#' @examples
#'	play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'	Game Loop
#' @description
#'	This function loops the Monty Hall game 'n' times to allow for a summary statistics to be made regarding chance of winning.
#' @details
#'	This function will play the Monty Hall game a specified 'n' times and collect information regarding both strategy and won/lost
#'	games into a data frame. It will then pipe this information into a proportion table, rounded to two decimal points. 
#' @param 
#'	This function takes in an input of a specified 'n'. 
#' @return 
#'	This function will return a table containing the proportions of wins/losses for each respective strategy (switch or stay) rounded
#'	to two place.
#' @examples
#'	play_n_games(n=100)
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
