powiat_per_image <- c(55, 55, 55, 55, 55, 48, 56)

letters <- list()

for (i in 1:2) {
  letters[[i]] <- list()
  for (j in 1:11) {
      color <- ifelse(i==1, 'b', 's')
      name <- paste0('litery/', color,  '/', j - 1, color, '.png')
      letters[[i]][[j]] <- readPNG(name)
  }
}


detect_digit <- function(img, line, j){
  
  if (line %% 2 == 1) { 
    l1p_r <- 6 + (line + 1) / 2 * 35
    l1k_r <- 11 + (line + 1) / 2 * 35
  
  } else {
    l1p_r <- 24 + floor(line / 2) * 35
    l1k_r <- 29 + floor(line / 2) * 35
  }

  l1p_c <- 1867 + j * 5
  l1k_c <- 1871 + j * 5
  
  if (j<4) {
    l1p_c  <- l1p_c - 3
    l1k_c  <- l1k_c - 3 
  }
  
  l1 <- img[l1p_r:l1k_r,l1p_c:l1k_c, 1:3]
  
  m <- Inf
  mj <- 100

 
  
  for (j in 1:11) {
    
    l2 <- letters[[(2 -line %%2)]][[j]][,,1:3]
    r = sum((l1-l2)^2)
    
    if (r < m) {
      m <- r
      mj <- j
    }
  }

  return(mj - 1)

}

detect_number <- function(img, line) {
   number <- 0
    
   all_10 <- TRUE
  
   for (i in 1:6) {
      digit <- detect_digit(img, line, i)
      if (digit != 10) {
        number <- 10 * number + digit
        all_10 <- FALSE
      }
   }
   
   if (all_10) return(NA)
   
   return(number)
}


df <- as.data.frame(powiat_names)

causes_of_death <- c('C00-D48', 'E10-E14', 'I21-I22', 'J00-J99')


for (folder_name in causes_of_death) { 
  v <- c()
  
  for (i in 1:7) {
      img <- readPNG(paste0('DATA/',folder_name, '/', i ,'.png'))
    
      n <- sapply(1:powiat_per_image[i], function(i) (detect_number(img, i)) )
      v <- c(v, n)
      
      print(n)
  }

  df <- cbind(df, v)
}

colnames(df) <- c('nazwa', 'nowotwory', 'cukrzyca', 'zawal.serca', 'niewyd.ukl.oddech')