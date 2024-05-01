library(palettes)

bccmha_palettes_discrete <- pal_palette(

  branding <- c("#3179b4",
                "#85c7d7",
                "#91b632",
                "#ff8d13",
                "#ef4c55"),

  likert <- c("steelblue", "#A7BED9", "grey90", "#FFB09D", "tomato2")

)

names(bccmha_palettes_discrete) <-  c("SATF", "likert")

plot(bccmha_palettes_discrete)

usethis::use_data(bccmha_palettes_discrete, overwrite = TRUE)
