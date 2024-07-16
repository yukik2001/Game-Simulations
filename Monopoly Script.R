library(R6)

# gameboard and decks -----------------------------------------------------
# Written by the professor
gameboard <- data.frame(
  space = 1:40, 
  title = c(
    "Go", "Mediterranean Avenue", "Community Chest", "Baltic Avenue",
    "Income Tax", "Reading Railroad", "Oriental Avenue", "Chance",
    "Vermont Avenue", "Connecticut Avenue", "Jail", "St. Charles Place",
    "Electric Company", "States Avenue", "Virginia Avenue",
    "Pennsylvania Railroad", "St. James Place", "Community Chest",
    "Tennessee Avenue", "New York Avenue", "Free Parking",
    "Kentucky Avenue", "Chance", "Indiana Avenue", "Illinois Avenue",
    "B & O Railroad", "Atlantic Avenue", "Ventnor Avenue", "Water Works",
    "Marvin Gardens", "Go to jail", "Pacific Avenue",
    "North Carolina Avenue", "Community Chest", "Pennsylvania Avenue",
    "Short Line Railroad", "Chance", "Park Place", "Luxury Tax",
    "Boardwalk"), stringsAsFactors = FALSE)
chancedeck <- data.frame(
  index = 1:15, 
  card = c(
    "Advance to Go", "Advance to Illinois Ave.",
    "Advance to St. Charles Place", "Advance token to nearest Utility",
    "Advance token to the nearest Railroad",
    "Take a ride on the Reading Railroad",
    "Take a walk on the Boardwalk", "Go to Jail", "Go Back 3 Spaces",
    "Bank pays you dividend of $50", "Get out of Jail Free",
    "Make general repairs on all your property", "Pay poor tax of $15",
    "You have been elected Chairman of the Board", 
    "Your building loan matures"), stringsAsFactors = FALSE)
communitydeck <- data.frame(
  index = 1:16, 
  card = c(
    "Advance to Go", "Go to Jail",
    "Bank error in your favor. Collect $200", "Doctor's fees Pay $50",
    "From sale of stock you get $45", "Get Out of Jail Free",
    "Grand Opera Night Opening", "Xmas Fund matures", "Income tax refund",
    "Life insurance matures. Collect $100", "Pay hospital fees of $100",
    "Pay school tax of $150", "Receive for services $25",
    "You are assessed for street repairs",
    "You have won second prize in a beauty contest",
    "You inherit $100"), stringsAsFactors = FALSE)

# RandomDice class --------------------------------------------------------
# Written by the professor

RandomDice <- R6Class(
  classname = "RandomDice",
  public = list(
    verbose = NA,
    initialize = function(verbose = FALSE){
      stopifnot(is.logical(verbose))
      self$verbose = verbose
    },
    roll = function() {
      outcome <- sample(1:6, size = 2, replace = TRUE)
      if(self$verbose){
        cat("Dice Rolled:", outcome[1], outcome[2], "\n")
      }
      outcome
    }
  )
)

# Preset Dice -------------------------------------------------------------
# Written by the professor

PresetDice <- R6Class(
  classname = "PresetDice",
  public = list(
    verbose = NA,
    preset_rolls = double(0),
    position = 1,
    initialize = function(rolls, verbose = FALSE){
      stopifnot(is.logical(verbose))
      stopifnot(is.numeric(rolls))
      self$preset_rolls = rolls
      self$verbose = verbose
    },
    roll = function(){
      if(self$position > length(self$preset_rolls)){
        stop("You have run out of predetermined dice outcomes.")
      }
      outcome <- c(self$preset_rolls[self$position], 
                   self$preset_rolls[self$position + 1])
      self$position <- self$position + 2
      if(self$verbose){
        cat("Dice Rolled:", outcome[1], outcome[2], "\n")
      }
      outcome
    }
  )
)


# Chance and Community Decks ----------------------------------------------
# Written by the professor

# This R6 class object shuffles the card deck when initialized.
# It has one method $draw(), which will draw a card from the deck.
# If all the cards have been drawn (position = deck length), then it will
# shuffle the cards again.
# The verbose option cats the card that is drawn on to the screen.
CardDeck <- R6Class(
  classname = "CardDeck",
  public = list(
    verbose = NA,
    deck_order = double(0), 
    deck = data.frame(),
    position = 1,
    initialize = function(deck, verbose = FALSE){
      stopifnot(is.data.frame(deck),
                is.numeric(deck[[1]]),
                is.character(deck[[2]]))
      self$deck_order <- sample(length(deck[[1]]))
      self$verbose <- verbose
      self$deck <- deck
    },
    draw = function(){
      if(self$position > length(self$deck_order)){
        # if we run out of cards, shuffle deck
        # and reset the position to 1
        if(self$verbose){
          cat("Shuffling deck.\n")
        }
        self$deck_order <- sample(length(self$deck[[1]]))
        self$position <- 1
      }
      outcome <- c(self$deck_order[self$position]) # outcome is the value at position
      self$position <- self$position + 1 # advance the position by 1
      if(self$verbose){
        cat("Card:", self$deck[outcome, 2], "\n")
      }
      outcome # return the outcome
    }
  )
)


# R6 Class SpaceTracker ---------------------------------------------------
# Written by the professor

SpaceTracker <- R6Class(
  classname = "SpaceTracker",
  public = list(
    counts = rep(0, 40),
    verbose = TRUE,
    tally = function(x){
      self$counts[x] <- self$counts[x] + 1
      if(self$verbose){
        cat("Added tally to ", x, ": ", gameboard$title[x], ".\n", sep = "")
      }
    },
    initialize = function(verbose){
      self$verbose <- verbose
    }
  )
)

# R6 Class Player ---------------------------------------------------------
# Written by myself

Player <- R6Class(
  classname = "Player",
  public = list(
    pos = 1,
    verbose = TRUE,
    count = 0,
    chance_n = 0,
    chest_n = 0,
    jail_n = 0,
    
    move_fwd = function(n){
      if (self$jail_n == 0) {
        if(self$verbose){
          cat("Player starts at ", self$pos, ": ", gameboard$title[self$pos], ".\n", sep = "")
          cat("Player moves forward ", n, ".\n", sep ="")
        }
        
        self$pos <- self$pos + n
        if(self$pos > 40){
          self$pos <- self$pos - 40
        }
        if(self$verbose){
          cat("Player is now at ", self$pos, ": ", gameboard$title[self$pos], ".\n", sep = "")
        }
        
        # Jail
        if(self$pos == 31) {
          if(self$verbose){
            cat("Player goes to jail.\n")
          }
          self$pos <- 11
          self$jail_n <- 1
        }
      }
    },
    
    chance = function(draw, card) {
      if(self$pos == 8 | self$pos == 23 | self$pos == 37 ) {
        self$chance_n <- 0
        if(self$verbose){
          cat("Darw a Chance card.\n")
        }
        draw
        
        # Chance position
        if(card == 1) {
          self$pos <- 1
        }
        if(card == 2) {
          self$pos  <- 25
        }
        if(card == 3) {
          self$pos <- 12
        }
        if(card == 4) {
          ifelse(self$pos == 23, self$pos <- 29, self$pos <- 13)
        }
        if(card == 5) {
          ifelse(self$pos == 8, self$pos <- 16, 
                 ifelse(self$pos == 23, self$pos <- 26, self$pos <- 6))
        }
        if(card == 6) {
          self$pos <- 6
        }
        if(card == 7) {
          self$pos <- 40
        }
        if(card == 8) {
          self$pos  <- 11
          self$jail_n <- 1
        }
        if(card == 9) {
          self$pos  <- self$pos  - 3
        }
        
        if(self$pos != 8 & self$pos != 23 & self$pos != 37 ) {
          if(self$verbose){
            cat("Player moves to ", self$pos, ": ", gameboard$title[self$pos], ".\n", sep = "")
          }
          self$chance_n <- 1
        }
      }
    },
    
    chest = function(draw, card) {
      if(self$pos == 3 | self$pos == 18 | self$pos == 34) {
        self$chest_n <- 0
        if(self$verbose){
          cat("Darw a Community Chest card.\n")
        }
        draw
        
        # Chest position
        if(card == 1) {
          self$pos <- 1
        }
        if(card == 2) {
          self$pos  <- 11
          self$jail_n <- 1
        }
        if(self$pos != 3 & self$pos != 18 & self$pos != 34 ) {
          if(self$verbose){
            cat("Player moves to ", self$pos, ": ", gameboard$title[self$pos], ".\n", sep = "")
          }
          self$chest_n <- 1
        }
      }
    },
    
    double = function(rolls) {
      if(self$jail_n == 0){
        if(rolls[1] == rolls[2]){
          self$count <- self$count + 1
          if(self$verbose){
            cat("Doubles count is now ", self$count, '.\n', sep = "")
          }
          if(self$count == 3) {
            if(self$verbose){
              cat("Player goes to jail.\n")
            }
            self$pos <- 11
            self$jail_n <- 1
          }
        }
      }
    },
    
    jail = function(rolls){
      if(self$jail_n == 3) {
        if(self$verbose){
          cat("Player's third turn in jail. Player must exit jail.\n")
          cat("Player exits jail.\n")
        }
        self$jail_n <- 0
      }
      if(self$jail_n >= 1) {
        if(rolls[1] == rolls[2]) {
          if(self$verbose){
            cat("In jail but rolled doubles.\n")
            cat("Player exits jail.\n")
          }
          self$jail_n <- 0
        }
        if(rolls[1] != rolls[2]) {
          if(self$verbose){
            cat("Player stays in jail.\n")
          }
          self$jail_n <- self$jail_n + 1
        }
      }
    },
    end = function(){
      self$count <- 0
      self$chance_n <- 0
      self$chest_n <- 0
    },
    
    initialize = function(verbose = FALSE, pos = 1) {
      self$verbose <- verbose
      self$pos <- pos
    }
  )
)


# VERY BASIC turn taking example ------------------------------------------
# Written by myself

take_turn <- function(player, spacetracker){
  dice_rolls <- dice$roll()
  player$double(dice_rolls)
  player$jail(dice_rolls)
  player$move_fwd(sum(dice_rolls))
  spacetracker$tally(player$pos)
  
  #Chance
  player$chance(chance$draw(), 
                chance$deck[chance$deck_order[chance$position - 1], 1])
  if(player$chance_n != 0) {spacetracker$tally(player$pos)}
  #Chest
  player$chest(community$draw(), 
               community$deck[community$deck_order[community$position - 1], 1])
  if(player$chest_n != 0) {spacetracker$tally(player$pos)}
  
  #Double
  double_check <- function(dice_rolls) {
    if(dice_rolls[1] == dice_rolls[2] & player$jail_n == 0){
      if(player$verbose){
        cat("\nPlayer rolled doubles, so they take another turn.\n")
      }
      dice_rolls <- dice$roll()
      player$double(dice_rolls)
      if(player$count < 3) {
        player$move_fwd(sum(dice_rolls))
        spacetracker$tally(player$pos)
        player$chance(chance$draw(), 
                      chance$deck[chance$deck_order[chance$position - 1], 1])
        if(player$chance_n != 0) {spacetracker$tally(player$pos)}
        player$chest(community$draw(), 
                     community$deck[community$deck_order[community$position - 1], 1])
        if(player$chest_n != 0) {spacetracker$tally(player$pos)}
        double_check(dice_rolls)
      } else {
        spacetracker$tally(player$pos)
      }
    }
  }
  
  if(player$count >= 1) {
    double_check(dice_rolls)
  }
  player$end()
}

