# install.packages("Ecdat")
library(Ecdat)

data("Cigar")

### Common Trend Assumptionの為に分析から特定の州を外す
### タバコの税金が1988年以降50セント以上上がった州のリスト
### Alaska, Hawaii, Maryland, Michigan, New Jersey, New York, Washington
skip_state <- c(3,9,10,22,21,23,31,33,48)

### Cigarデータセットの読み込み --------------------------------------------
### skip_stateに含まれる州のデータを削除
Cigar <- Cigar %>%
  filter(!state %in% skip_state,
         year >= 70) %>%
  mutate(area = if_else(state == 5, "CA", "Rest of US"))

## 前後比較による分析 --------------------------------------------
Cigar %>%
  mutate(period = if_else(year > 87, "after", "before"),
         state = if_else(state == 5, "CA", "Rest of US")) %>%
  group_by(period, state) %>%
  summarise(sales = sum(sales*pop16)/sum(pop16)) %>%
  tidyr::spread(state, sales)

## 前後比較のプロット --------------------------------------------
Cigar %>%
  mutate(period = if_else(year > 87, "after", "before"),
         state = if_else(state == 5, "CA", "Rest of US")) %>%
  group_by(period, state) %>%
  summarise(sales = sum(sales*pop16)/sum(pop16)) %>%
  ggplot(aes(y = sales,
             x = period,
             shape = state,
             linetype = state)) +
  geom_point(size = 2) +
  geom_line(aes(group = state)) +
  ylim(0, NA) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(1,1,1,1, "cm")) +
  scale_x_discrete(name ="Period",limits=c("before","after"))
ggsave("./answer/BAplot_DID.png")


## タバコの売上のトレンドを示すプロット --------------------------------------------
Cigar %>%
  mutate(state = if_else(state == 5, "CA", "Rest of US")) %>%
  group_by(year,state) %>%
  summarise(sales = sum(sales*pop16)/sum(pop16)) %>%
  ggplot(aes(y = sales,
             x = year,
             shape = state,
             linetype = state)) +
  geom_line() +
  geom_point(size = 2) +
  geom_vline(xintercept = 88, linetype = 4) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(1,1,1,1, "cm"))
ggsave("./answer/parallel_trends_DID.png")
