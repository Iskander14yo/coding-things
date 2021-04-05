decomp <- function(n) {
  prim_vec = 2:n
  # составим массив из простых чисел
  simple_numbers = upd_upd(n)
  # Разложим на делители
  vec = integer(length(simple_numbers))
  # пусть prim_vec = c(2,3,4,5,6)
  # simple_numbers = c(2,3,5)
  vec = decomp_method(n, simple_numbers)
  
  simple_numbers = simple_numbers[1:length(vec)]
  if (any(vec == 1)) {
    index = which(vec == 1)[1]
    vec1 = vec[1:(index - 1)]
    vec2 = vec[index:length(vec)]
    simple_numbers1 = simple_numbers[1 : (index - 1)]
    simple_numbers2 = simple_numbers[index : length(simple_numbers)]
    itog1 = paste(simple_numbers1, vec1, collapse = ' * ', sep = '^')
    itog2 = paste(rep(' * ', length(simple_numbers2)), simple_numbers2, collapse = '', sep = '')
    return(paste(itog1, itog2, sep = ''))
  } else {
    return(paste(simple_numbers, vec, col = ' * ', sep = '^'))
  }  
}

simple <- function(n){
  prim_vec = 2:n
  simple_numbers = c(2)
  for (i in prim_vec){
    k = 0
    num = i
    for (j in (num-1):2){
      if (num %% j != 0) next
      k = 1
    }
    if (k == 0) simple_numbers = c(simple_numbers, num)  
  }
  return(simple_numbers)
}

eratosphen <- function(n){
  vec = 2:n
  i = 2
  j = 2
  while (i*j <= n){
    while (i*j <= n){
      if (any(vec == i*j)){
        vec = vec[-which(vec == i*j)]
      } else {
        j = j + 1
        next
      }
    }
    j = 2
    i = i + 1
  }
  return(vec)
}

eratosphen_upd <- function(n){
  vec = 2:n
  vec = vec[vec %% 2 == 1]
  i = 2
  j = 2
  while (i*j <= n){
    if (i == 2) next
    if (i < 5){
      while (i*j <= n){
        if (any(vec == i*j)){
          vec = vec[-which(vec == i*j)]
        } else {
          j = j + 1
          next
        }
      }
      j = 2
      i = i + 1
    } else {
        j = i
        while (i*j <= n){
          if (any(vec == i*j)){
            vec = vec[-which(vec == i*j)]
          } else {
            j = j + 1
            next
          }
        }
        j = 2
        i = i + 1
  } }
  return(vec)
}

upd_upd <- function(n){
  prim_vec = 2:n
  for (i in 2:round(sqrt(n) + 1)){
    prim_vec[which(prim_vec %% i == 0)] = 0
    prim_vec[(which(prim_vec == 0))][1] = i 
    prim_vec = prim_vec[prim_vec != 0]
  }
  return(prim_vec)
}

decomp_method <- function(n, simple_numbers){
  prim_vec = 2:n
  simple_numbers1 = simple_numbers[which(simple_numbers * 2 <= (length(prim_vec) + 1))]
  simple_numbers2 = simple_numbers[!(simple_numbers %in% simple_numbers1)]
  prim_vec = prim_vec[!(prim_vec %in% simple_numbers2)]
  vec = integer(length(simple_numbers1))
  while(all(prim_vec != 1)){
    for (i in simple_numbers1){
      index = which(simple_numbers1 == i)
      while (any(prim_vec %% i == 0)){
        l = length(prim_vec[which(prim_vec %% i == 0)])
        prim_vec[which(prim_vec %% i == 0)] = prim_vec[prim_vec %% i == 0] / i
        vec[index] = vec[index] + l
      }
    }
  }
  vec = c(vec, rep(1, length(simple_numbers2)))
  return(vec)
}

decomp_wars <- function(n) {
  prim_vec = 2:n
  # составим массив из простых чисел
  simple_numbers = eratosphen(n)
  # Разложим на делители
  vec = integer(length(simple_numbers))
  # пусть prim_vec = c(2,3,4,5,6)
  # simple_numbers = c(2,3,5)
  for (i in prim_vec){
    num = i
    for (j in simple_numbers){
      index = which(simple_numbers == j)
      while (num %% j == 0){
        num = num / j
        vec[index] = vec[index] + 1
      }
    }}
  
  simple_numbers = simple_numbers[1:length(vec)]
  if (any(vec == 1)) {
    index = which(vec == 1)[1]
    vec1 = vec[1:(index - 1)]
    vec2 = vec[index:length(vec)]
    simple_numbers1 = simple_numbers[1 : (index - 1)]
    simple_numbers2 = simple_numbers[index : length(simple_numbers)]
    itog1 = paste(simple_numbers1, vec1, collapse = ' * ', sep = '^')
    itog2 = paste(rep(' * ', length(simple_numbers2)), simple_numbers2, collapse = '', sep = '')
    return(paste(itog1, itog2, sep = ''))
  } else {
    return(paste(simple_numbers, vec, col = ' * ', sep = '^'))
  }  
}

decomp_eratosphen <- function(n) {
  prim_vec = 2:n
  # составим массив из простых чисел
  simple_numbers = eratosphen(n)
  # Разложим на делители
  vec = integer(length(simple_numbers))
  for (i in prim_vec){
    num = i
    for (j in simple_numbers){
      index = which(simple_numbers == j)
      while (num %% j == 0){
        num = num / j
        vec[index] = vec[index] + 1
      }
    }}
  simple_numbers = simple_numbers[1:length(vec)]
  if (any(vec == 1)) {
    index = which(vec == 1)[1]
    vec1 = vec[1:(index - 1)]
    vec2 = vec[index:length(vec)]
    simple_numbers1 = simple_numbers[1 : (index - 1)]
    simple_numbers2 = simple_numbers[index : length(simple_numbers)]
    itog1 = paste(simple_numbers1, vec1, collapse = ' * ', sep = '^')
    itog2 = paste(rep(' * ', length(simple_numbers2)), simple_numbers2, collapse = '', sep = '')
    return(paste(itog1, itog2, sep = ''))
  } else {
    return(paste(simple_numbers, vec, col = ' * ', sep = '^'))
  }  
}

decomp_simple <- function(n) {
  prim_vec = 2:n
  # составим массив из простых чисел
  simple_numbers = simple(n)
  # Разложим на делители
  vec = integer(length(simple_numbers))
  for (i in prim_vec){
    num = i
    for (j in simple_numbers){
      index = which(simple_numbers == j)
      while (num %% j == 0){
        num = num / j
        vec[index] = vec[index] + 1
      }
    }}
  simple_numbers = simple_numbers[1:length(vec)]
  if (any(vec == 1)) {
    index = which(vec == 1)[1]
    vec1 = vec[1:(index - 1)]
    vec2 = vec[index:length(vec)]
    simple_numbers1 = simple_numbers[1 : (index - 1)]
    simple_numbers2 = simple_numbers[index : length(simple_numbers)]
    itog1 = paste(simple_numbers1, vec1, collapse = ' * ', sep = '^')
    itog2 = paste(rep(' * ', length(simple_numbers2)), simple_numbers2, collapse = '', sep = '')
    return(paste(itog1, itog2, sep = ''))
  } else {
    return(paste(simple_numbers, vec, col = ' * ', sep = '^'))
  }  
}

is.prime <- function(n) n == 2L || all(n %% 2L:ceiling(sqrt(n)) != 0)
decomp <- function(n) {
  primes <- which(sapply(1:n, is.prime))
  factors <- sapply(primes, function(p) {
    m <- floor(log(n, base = p))
    paste0(p, '^', sum(floor(n / (p ** (1:m)))))
    
  })
  paste(sub(pattern = '\\^1$', replacement = '', x = factors), collapse = ' * ')
}



df <- matrix(nrow = 2, ncol = 7)
df <- as.data.frame(df)
colnames(df) = c(10, 50, 100, 500, 1000, 2000, 4000)
row.names(df) = c('simple', 'eratosphen')
df[1,] = map(c(10, 50, 100, 500, 1000, 2000, 4000), function(x){microbenchmark(simple(x), times = 1)$time})
df[2,] = map(c(10, 50, 100, 500, 1000, 2000, 4000), function(x){microbenchmark(eratosphen(x), times = 1)$time})

df[1,] = map(c(10, 50, 100, 500, 1000, 2000, 4000), function(x){microbenchmark(decomp_simple(x), times = 1)$time})
df[2,] = map(c(10, 50, 100, 500, 1000, 2000, 4000), function(x){microbenchmark(decomp_eratosphen(x), times = 1)$time})
df[3,] = map(c(10, 50, 100, 500, 1000, 2000, 4000), function(x){microbenchmark(decomp_wars(x), times = 1)$time})
