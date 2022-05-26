## -----------------------------------------------------------------------------
print_pattern <- function(counts = c(3, 2, 1, 2), repeats=4) {
  for (i in seq_len(repeats)) {
    for (c in counts) {
        for (j in 1:c)
          cat(1)
        cat(0)
    }
  }
  cat("\n")
}

## -----------------------------------------------------------------------------
print_pattern(repeats=4);

## -----------------------------------------------------------------------------
library(async) # for gen
gen_pattern <- function(counts = c(3, 2, 1, 2)) { force(counts)
  gen({
    repeat {
      for (n in counts) {
        for (j in 1:n)
          yield(1)
        yield(0)
      }
    }
  })
}

## -----------------------------------------------------------------------------
library(iterators)
p <- gen_pattern()
for (i in 1:23) { cat(nextElem(p)) }; cat("\n")

## -----------------------------------------------------------------------------
library(magrittr)
show_head <- function(x, n=24) {
  x %>% itertools::ilimit(n) %>% as.list() %>% deparse() %>% cat(sep="\n")
}
show_head(gen_pattern(), 24)

## ---- eval=FALSE--------------------------------------------------------------
#  tmp <- tempdir()
#  baseurl <- "https://github.com/octoblu/drum-kit/raw/master/public/assets/samples"
#  samplepaths <- paste0(tmp, c("x" = "/clap4.wav","X" = "/clap5.wav"))
#  curl::curl_download(paste0(baseurl, "/clap%20(4).WAV"), samplepaths[1])
#  curl::curl_download(paste0(baseurl, "/clap%20(5).WAV"), samplepaths[2])

## ---- eval=FALSE--------------------------------------------------------------
#  library(audio) # for load.wave, play
#  claps <- lapply(samplepaths, load.wave)
#  play(claps[[1]])
#  play(claps[[2]])

## ---- eval=FALSE--------------------------------------------------------------
#  gen_pattern() %>% itertools::ilimit(36) %>% iplay(claps, 360)

## -----------------------------------------------------------------------------
drop_one_after <- function(g, n, sep=character(0)) {  list(g, n, sep)
  gen(
    repeat {
      for (i in 1:n) yield(nextElem(g))
      nextElem(g) #drop
      cat(sep) # print a seperator after every skip
    }
  )
}

## ---- eval=FALSE--------------------------------------------------------------
#  iplay(clapping_music(n=4, sep="\n"), claps, 480)

