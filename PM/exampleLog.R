#PM - example Log
#https://cran.r-project.org/web/packages/processanimateR/readme/README.html
library(processanimateR)
library(eventdataR)


load("data/example_log.rda")
example_log
example_log %>% traces()

animate_process(example_log)
animate_process(example_log, mapping = token_aes(color = token_scale("red")))
animate_process(example_log, mapping = token_aes(size = token_scale(12), shape = "rect"))

m1 <- animate_process(example_log,  mapping = token_aes(shape = "image", size = token_scale(10),  image = token_scale("https://upload.wikimedia.org/wikipedia/en/5/5f/Pacman.gif")))

m1 <- animate_process(example_log)
htmlwidgets::saveWidget(m1, file = "test.mp4")
#should work.#You can also embed it into an Rmarkdown report with knitR with some caveats regarding auto-sizing.

#https://yihui.org/animation/examples/