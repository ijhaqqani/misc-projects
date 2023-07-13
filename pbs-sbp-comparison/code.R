library(tidyverse)
library(readxl)
library(ggbraid)
library(patchwork)
setwd("C:/Users/ijlal/OneDrive/Documents/Personal/Portfolio/misc-projects/pbs-sbp-comparison")

# initial prep of data
old_pbs_data <- 
  read_excel("./pbs_sbp_trade.xlsx",sheet = "old data") %>% 
  mutate(type = "pbs") %>% 
  distinct(.keep_all = T) %>% 
  select(y,m,e,i,type) %>% 
  left_join(
    read_csv("./exchange_rates.csv") %>%
      janitor::clean_names() %>%
      filter(series_key == "TS_GP_ER_FAERPKR_M.E00220") %>%
      mutate(y = year(dmy(observation_date)),
             m = month(dmy(observation_date))) %>%
      select(y, m, er = observation_value)
  ) %>% 
  mutate(e = e/er,
         i = i/er) %>% 
  select(-er)

pbs <- 
  read_excel("./pbs_sbp_trade.xlsx",sheet = "Sheet1") %>% 
  bind_rows(old_pbs_data) %>% 
  # distinct(.keep_all = T)
  mutate(date = ymd(paste(y,m,"01",sep = "-")) %>% ceiling_date(unit = "months") %>% rollback()) %>% 
  select(date,exports = e, imports = i) %>% 
  pivot_longer(cols = c(exports,imports),names_to = "head",values_to = "pbs")

sbp <- 
  read_csv("./bop.csv") %>% 
  mutate(date = dmy(`Observation Date`)) %>% 
  janitor::clean_names() %>% 
  filter(series_key %in% c("TS_GP_BOP_BPM6SUM_M.P00030",
                           "TS_GP_BOP_BPM6SUM_M.P00040")
  ) %>% 
  # date>=ymd("2022-01-01")) %>% 
  select(date, sbp = observation_value,head = series_name) %>% 
  mutate(
    # type = "sbp",
    head = if_else(str_detect(head,"Exports"),"exports","imports")
  )

combined_data <- 
  pbs %>% 
  full_join(
    sbp,by = c("date","head")
  ) %>% 
  drop_na() %>% 
  arrange(desc(date)) %>% 
  pivot_longer(cols = -1:-2,names_to = "inst") %>% 
  mutate(date = ymd(paste(year(date),month(date),1,sep = "-"))) %>% 
  {
    d1 <- .
    d1 %>% 
      bind_rows(
        d1 %>% 
          group_by(date,inst) %>% 
          summarize(value = value[head=="exports"] - value[head=="imports"],.groups = "drop") %>% 
          mutate(head = "Balance of Trade")
      )
  }

library(colorspace)
colors <- c("sandybrown","lightslateblue")
fills <- desaturate(lighten(colors, .3), .3)

# prep of data and plots of imports, exports and BoT plots, showing comparison of SBP and PBS
head_wise_plots <- 
  combined_data%>% 
  split(.$head) %>% 
  imap(function(d,y){
    # browser()
    p <- 
      d %>% 
      ggplot()+
      geom_line(aes(x = date, y = value,color = inst),linewidth = 0.85)+
      geom_braid(
        method = "line",
        data = 
          d %>% 
          select(-head) %>% 
          pivot_wider(names_from = inst),
        aes(x = date, ymin = pbs, ymax = sbp, fill = pbs<sbp),alpha = 0.5
      )+
      scale_color_manual(values = colors,labels = c("PBS","SBP"))+
      scale_fill_manual(values = fills)+
      theme_bw()+
      guides(fill = "none")+
      labs(x = NULL,y = NULL,color = NULL,title = d$head %>% unique() %>% str_to_sentence())+
      theme(
        legend.position = c(0.2,0.8),
        legend.text = element_text(size = 12,face = "bold"),
        legend.key.size = unit(32,"pt"),
        plot.title = element_text(size = 14,face = "bold")
      )
    if (y=="imports") {
      p <- 
        p+
        theme(
          legend.position = "none"
        )
    } else {
      p <- 
        p+
        theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
    }
  })

# prep and export of plots of trade comparison
exports_vs_imports_plot <- 
  head_wise_plots %>% 
  .[2:3] %>% # excluding balance of trade
  wrap_plots(ncol = 1)+
  plot_annotation(
    title = "Exports and Imports - A comparison of SBP and PBS data",
    subtitle = "Amounts are in million USD",
    theme = theme(
      plot.title = element_text(size = 16,face = "bold",hjust = 0.5,margin = margin(t = 10)),
      plot.subtitle = element_text(hjust = 0.5,margin = margin(t = 5),size = 12)
    )
  )&
  theme(
    axis.text = element_text(size = 10.5)
  )

ggsave(plot = exports_vs_imports_plot,"exports vs imports.png",width = 12, height = 12)
  
# plot on Balance of trade
bot_plot <- 
  head_wise_plots %>% 
  .[[1]] + 
  theme(
  legend.position = c(0.2,0.2),
  plot.title = element_text(hjust = 0.5, 
                            margin = margin(t = 10, b = 10)),
  axis.text.x = element_text(size = 10)
)+
  labs(y = "Million $")

bot_plot %>% 
  ggsave(plot = .,"balance_of_trade.png",width = 10, height = 7,dpi = 600)

# incorporation of CAB data
cab <- 
  read_csv("./bop.csv") %>% 
  janitor::clean_names() %>% 
  filter(series_key == "TS_GP_BOP_BPM6SUM_M.P00010") %>% 
  mutate(
    date = dmy(observation_date),
    date = ymd(paste(year(date),month(date),1,sep = "-"))
    ) %>% 
  select(date,cab = observation_value) %>% 
  right_join(
    combined_data %>% 
      filter(head=="Balance of Trade") %>% 
      select(-head) %>% 
      pivot_wider(names_from = inst),
    by = c("date")
  ) %>% 
  mutate(
    m_p = (sbp+pbs)/2,
    ymin = m_p - (cab/2),
    ymax = m_p + (cab/2),
  )

combined_data2 <- 
  combined_data %>% 
  filter(head=="Balance of Trade") %>% 
  mutate(era = case_when(
    date<=ymd("2017-6-30")~"Era 1",
    (date > ymd("2017-6-30")) & (date<=ymd("2021-5-31"))~"Era 2",
    date > ymd("2020-8-31")~"Era 3"
  ))

# prep of cab/cad plot
cab_plot <- 
  combined_data2%>% 
  ggplot()+
  geom_line(aes(x = date,
                y = value,
                # color = type,
                color = inst
  ),
  color = "white",
  size = 3.2
  )+
  geom_line(aes(x = date,
                y = value,
                color = inst,
                # group = type
  ),
  size = 1.2
  )+
  geom_braid(
    method = "line",
    data =
      combined_data2 %>% 
      filter(head=="Balance of Trade") %>% 
      select(-head) %>% 
      pivot_wider(names_from = inst),
    aes(date,ymin = pbs,ymax = sbp,fill = pbs<sbp),alpha = 0.5
  )+
  geom_errorbar(
    data = 
      cab %>% 
      filter(date>=min(combined_data$date)) %>% 
      mutate(era = case_when(
        date<=ymd("2017-6-30")~"Era 1",
        (date > ymd("2017-6-30")) & (date<=ymd("2021-5-31"))~"Era 2",
        date > ymd("2020-8-31")~"Era 3"
      )),
    aes(x = date, ymin = ymin, ymax = ymax),
    width = 10,
    alpha = 0.9
  )+
  scale_color_manual(values = colors,labels = c("PBS","SBP"))+
  scale_fill_manual(values = fills,
                    guide = "none")+
  facet_wrap(.~era,scales = "free",ncol = 1,
             labeller = labeller(era = c(
               "Era 1" = "Era 1 (Exchange rate overvaluation)",
               "Era 2" = "Era 2 (Exchange rate policy shift",
               "Era 3" = "Era 3 (Post-covid inflation and growth boom)"
             ))
             )+
  scale_x_date(date_labels = "%Y")+
  theme_bw()+
  labs(x = NULL,y = NULL,color = NULL)+
  theme(
    legend.position = c(0.2,0.82),
    legend.box.background = element_rect(fill = "transparent",color = NA),
    legend.background = element_rect(fill = "transparent",colour = NA),
    legend.text = element_text(size = 12,face = "bold"),
    legend.key.size = unit(30,"pt"),
    axis.text = element_text(size =13),
    strip.text = element_text(size = 14),
    plot.title = element_text(size = 16,face = "bold",margin = margin(t = 10, b = 5),hjust = 0.5),
    plot.subtitle = element_text(margin = margin(b = 10),hjust = 0.5)
  )+
  ggtitle(
    label = "Current Account Balance in comparison with Trade Deficits of SBP and PBS",
    subtitle = "Amount in million USD"
  )

cab_plot %>% ggsave(plot = .,"cab_plot.png",width = 11,height = 11,dpi = 600)

# scatter between cad and sbp-pbs
library(ggpubr)
scatter_cab <-
  cab %>% 
  ggplot()+
  geom_point(aes(x = -cab,y = sbp-pbs))+
  stat_smooth(method = "lm",aes(x = -cab, y = sbp-pbs))+
  labs(x = "Current Account Deficit", y = "SBP Trade Deficit - PBS Trade Deficit")+
  theme_bw()+
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(size = 15,face = "bold",margin = margin(t = 10)),
    plot.subtitle = element_text(margin = margin(b = 10))
  )+
  ggtitle(
    label = "Relationship between CAB and SBP/PBS Trade Deficits",
    subtitle = "Amount in million USD"
  )

scatter_cab %>% ggsave(plot = ., filename = "scatter_cab.png",width = 9,height = 7,dpi = 600)  

