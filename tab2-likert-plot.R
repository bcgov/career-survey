library(ggplot2)
library(dplyr)

data <- read.csv("data/BGS_SUCC_RESULTS_sample.csv") %>%
  filter(MINISTRY_NAME == "Ministry of Scarlet") %>%
  filter(WK_UNIT_NAME == "Frog Unit") %>% 
  filter(SRV_QU_GRP_CODE == 14) %>%
  dplyr::select(QU_TXT, RESP_TXT, RESP_CODE, RESP_PERCENT, SCALE_AVG) %>%
  mutate(RESP_PERCENT = RESP_PERCENT * 100) %>%
  mutate(QU_TXT_UI =
           unlist(lapply(
             stringi::stri_wrap(QU_TXT, width = 50,simplify = FALSE),
             paste, collapse = "\n"))) %>%
  droplevels

data$RESP_TXT <- factor(
  data$RESP_TXT,
  levels = rev(unique(data$RESP_TXT[order(data$RESP_CODE)])))

ggplot(data,
       aes(x = QU_TXT_UI, y = RESP_PERCENT,
           fill = RESP_TXT, label = RESP_PERCENT)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(aes(label = ifelse(RESP_PERCENT == 0, "",
                               sprintf("%s%%", round(RESP_PERCENT)))),
            color = "#11335b", fontface = "bold", size = 4,
            position = position_stack(vjust = 0.5)) +
  coord_flip() +
  theme(
    panel.background = element_rect(fill = "transparent"),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(colour = "#11335b"),
    legend.text = element_text(colour = "#11335b", face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  xlab(NULL) + ylab(NULL) +
  scale_fill_manual(values = c("#0081c1", "#4db447", "#ffb200"),
                    labels = paste0(levels(data$RESP_TXT), "    ")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_text(data = data, y = 105,
            aes(label = round(SCALE_AVG * 100)),
            position = "identity", vjust = 0.5, hjust = -0.5) +
  scale_y_continuous(limits = c(0, 105)) +
  ggtitle("Score \n/100  ") +
  theme(plot.title = element_text(hjust = 1))

