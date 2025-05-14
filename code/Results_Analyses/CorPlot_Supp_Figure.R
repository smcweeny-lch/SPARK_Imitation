library(psych)

data <- readRDS("./data/reverse_score_corrected_data_2_26_25.rds")

data <- data %>% select(q01_throw_ball:q15_fatigue_easily, q01_whole_body:q15_arranging, q16_complete, q17_washing, q18_checking, q19_counting, q20_hoarding, q21_repeating:q43_fascination_movement, q01_phrases:q40_cooperatively_games, everything())

scale <- 10
png("./Manuscript/Supplemental Materials/correlation_plot_all_items.png", width = 1080*scale, height = 960*scale, res = 72*scale)
corPlot(data, MAR = 5, xlas = 2, cex = .1, cex.axis = .7, scale = F)
dev.off()


names(data)
