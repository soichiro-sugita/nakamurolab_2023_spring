library(ggplot2)
library(psych)
library(dplyr)

#data("anscombe")
df <- read.csv('./data/in/anscombes.csv')

# grouping the data
summary <- df %>% 
  dplyr::group_by(dataset) %>% 
  summarise(x.mean = mean(x), # xの標本平均
            y.mean = mean(y), # yの標本平均
            x.stdev = sd(x),  # xの標準偏差
            y.stdev = sd(y))  # yの標準偏差

write.csv(summary, './answer/group_mean_stdev.csv')

# scatter plot------------------------------------------------------
p_scat <- ggplot2::ggplot(data = df, 
                          mapping = aes(x = x, y = y, colour = dataset)
                          ) + 
  ggplot2::geom_point() +
  ggplot2::facet_grid(.~ dataset)
p_scat
ggsave("./answer/scatter_plot.png")

# boxplot------------------------------------------------------
p_box <- ggplot2::ggplot(data = df, mapping = aes(x = x, y = y, fill = dataset)) +
  ggplot2::geom_boxplot()
p_box
ggsave("./answer/box_plot.png")

# histgram(carnel plot)------------------------------------------------------
# x
p_hist_x <- ggplot2::ggplot(data = df, mapping = aes(x = x, colour = dataset)) +
  ggplot2::geom_density() +
  ggplot2::facet_grid(. ~ dataset)
p_hist_x
ggsave("./answer/hist_x.png")

# y
p_hist_y <- ggplot2::ggplot(data = df, mapping = aes(x = y, colour = dataset)) +
  ggplot2::geom_density() +
  ggplot2::facet_grid(. ~ dataset)
p_hist_y
ggsave("./answer/hist_y.png")



