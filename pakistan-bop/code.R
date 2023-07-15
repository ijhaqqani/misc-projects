library(tidyverse)
library(readxl)
library(patchwork)

setwd("C:/Users/ijlal/OneDrive/Documents/Personal/Portfolio/misc-projects/pakistan-bop")

head_function <- function(data){
  data %>% 
    t() %>% 
    data.frame() %>% 
    tibble() %>% 
    fill(X1) %>% 
    replace_na(list(x2 = "")) %>% 
    # filter(!(str_detect(X2,"Jul"))) %>% 
    mutate(X3 = paste0(X1,"-",X2)) %>% 
    pull(X3)
}

date_m <- function(y,q){
  # browser()
  ymd(paste(
    paste0(if_else(str_sub(y, 3, 4) >=50, "19", "20"), str_sub(y, 3, 4)),
    3 * (str_sub(q, 2, 2) %>% as.numeric()),
    "01",
    sep = "-"
  )) %>%
    ceiling_date(unit = "months") %>%
    rollback()
}

data_compile_f <- function(filename){
  # browser()
  
  read_excel(filename,col_names = F,skip = 2) %>% 
    select(-1) %>% 
    set_names(c("id",read_excel(filename,col_names = F,n_max = 2) %>% head_function())) %>% 
    pivot_longer(cols = -1:-2,names_to = c("year","q"),names_sep = "-") %>% 
    filter(str_length(q)==2) %>% 
    rename(items = 2) %>% 
    filter(!is.na(items)) %>% 
    mutate(date = date_m(year,q))
}

bpm4 <- 
  data_compile_f("Balancepayment_BPM4-Arch.xlsx")

bpm6 <- 
  data_compile_f("Balancepayment_BPM6-Arch.xlsx")

mapping_bpm4 <- 
  tribble(
    ~items, ~c_items,
    "Export f.o.b.", "Exports",
    "Import f.o.b.", "Imports",
    "Services (net)", "Other flows in Current Account",
    "Workers remittance", "Workers Remittances",
    "FCAs residents", "Other flows in Current Account",
    "Others", "Other flows in Current Account",
    "Gross Disbursements", "Govt. Debt Disbursements",
    "Amortization", "Govt. Debt Amortizations",
    "other flows in capital account", "Other flows in Capital/Financial Account \n(incl. debt relief/assistance)",
    "V.Errors &Omissions (net)", "Net Errors and Omissions",
    "VII.Official assistance and debt relief", "Other flows in Capital/Financial Account \n(incl. debt relief/assistance)"
  ) %>% 
  mutate(
    c_items = factor(
      c_items,
      levels = c(
        "Exports",
        "Imports",
        "Workers Remittances",
        "Other flows in Current Account",
        "Govt. Debt Disbursements",
        "Govt. Debt Amortizations",
        "Other flows in Capital/Financial Account \n(incl. debt relief/assistance)",
        "Net Errors and Omissions"
      )
    )
  )

mapping_bpm6 <- 
  tribble(
    ~items,~c_items,
    "Exports of goods FOB",	"Exports",
    "Imports of goods FOB",	"Imports",
    "Balance on trade in services",	"Other flows in Current Account",
    "Balance on primary income",	"Other flows in Current Account",
    "Workers' remittances",	"Workers Remittances",
    "other flows in current account",	"Other flows in Current Account",
    "Direct investment",	"Investment flows",
    "Portfolio investment",	"Investment flows",
    "Disbursements",	"Govt. Debt Disbursements",
    "Amortization",	"Govt. Debt Amortizations",
    "Other flows in financial and capital accounts",	"Other flows in Capital/Financial Account \n(incl. debt relief/assistance)",
    "Net Errors and Omissions",	"Net Errors and Omissions"
  ) %>% 
  mutate(c_items = factor(
    c_items,
    levels = c(
      "Exports",
      "Imports",
      "Workers Remittances",
      "Other flows in Current Account",
      "Investment flows",
      "Govt. Debt Disbursements",
      "Govt. Debt Amortizations",
      "Other flows in Capital/Financial Account \n(incl. debt relief/assistance)",
      "Net Errors and Omissions"
    )
  ))


fills_bpm4 <- 
  mapping_bpm4 %>% 
  arrange(c_items) %>% 
  distinct(c_items) %>% 
  bind_cols(
    colors = c(
      "green4","indianred","lightblue4","darkblue",
      "gold2","magenta","cyan","sienna"
    )
  )

fills_bpm6 <- 
  mapping_bpm6 %>% 
  arrange(c_items) %>% 
  distinct(c_items) %>% 
  bind_cols(
    colors = c(
      "green4","indianred","lightblue4", "darkblue",
      "lavenderblush","gold2","magenta","cyan","sienna"
    )
  )

fills_bpm4
fills_bpm6

plotter <- function(data,bpm){
  if (bpm=="4") {
    fills <- fills_bpm4
  } else {
    fills <- fills_bpm6
  }
  data %>% 
    ggplot()+
    geom_col(
      aes(x = date, y = value, fill = c_items),
      position = position_stack()
    )+
    scale_fill_manual(values = fills$colors)+
    theme_bw()+
    labs(x = NULL,y = NULL,fill = NULL)+
    theme(
      legend.position = "bottom"
    )
  
}

combined_data <- 
  bpm4 %>% 
  filter(id==1) %>% 
  left_join(mapping_bpm4) %>% 
  group_by(c_items,date) %>% 
  summarize(value = sum(value,na.rm = T),.groups = "drop") %>% 
  bind_rows(
    bpm6 %>% 
      filter(id==1) %>% 
      left_join(mapping_bpm6) %>%
      group_by(c_items,date) %>% 
      summarize(value = sum(value,na.rm = T),.groups = "drop")
  ) %>% 
  mutate(c_items = factor(
    c_items,
    levels = c(
      "Exports",
      "Imports",
      "Workers Remittances",
      "Other flows in Current Account",
      "Investment flows",
      "Govt. Debt Disbursements",
      "Govt. Debt Amortizations",
      "Other flows in Capital/Financial Account \n(incl. debt relief/assistance)",
      "Net Errors and Omissions"
    )
  ))

# main chart
bop_plot <- 
  combined_data%>% 
  plotter(bpm = "6")+
  theme(
    legend.key.size = unit(20, "pt"),
    legend.position = c(0.4,0.88),
    legend.direction = "horizontal",
    legend.spacing.x = unit(5,"pt"),
    legend.text = element_text(hjust = 0),
    legend.background = element_rect(fill = "transparent"),
    plot.margin = margin(rep(20,4)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text = element_text(face = "bold")
  )+
  labs(
    # y = "Million USD",
    x = "Fiscal Year (July - June)")+
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y")+
  scale_y_continuous(
    expand = expansion(mult = c(0.45,0.2)),
    labels = scales::label_comma(),
    breaks = scales::breaks_width(5000))

# balances line chart (inset)
balances <- 
  bpm4 %>% 
  bind_rows(bpm6) %>% 
  filter(id==2) %>%
  filter(str_detect(items,"(?i)current|overall|trade")) %>% 
  mutate(items = 
    case_when(
      str_detect(items,"(?i)current")~"Current Account Balance",
      str_detect(items,"(?i)trade")~"Trade Balance",
      TRUE~"Overall Balance"
    )
  ) %>% 
  mutate(
    value = if_else(str_detect(items,"(?i)overall"),-value,value)
  ) %>% 
  mutate(
    items = factor(items,levels = c("Trade Balance","Current Account Balance","Overall Balance"))
  ) %>% 
  ggplot()+
  scale_y_continuous(expand = expansion(mult = c(0.2,0.15)))+
  geom_line(aes(x = date, y = value, color = items),linewidth = 1)+
  scale_color_manual(values = c("palevioletred","royalblue","mediumseagreen"))+
  theme_bw()+
  labs(x = NULL,y = NULL,fill = NULL,color = NULL)+
  theme(
    legend.key.size = unit(18,"pt"),
    legend.text = element_text(size = 10),
    legend.position = c(0.25,0.25)
  )

balances+
  labs(
    y = "Million USD",
    x = "Fiscal Year (July - June)")+
  ggsave("balances.png",width = 10,height = 7,dpi = 600)

# final export to png
bop_plot+
  inset_element(balances,0.05,0.03,0.6,0.4,align_to = "panel")+
  plot_annotation(
    title = "Pakistan's Balance of Payments",
    subtitle = "(all amounts are in million $)",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold",hjust = 0.5,margin = margin(t = 10)),
      plot.subtitle = element_text(size = 12, margin = margin(t = 5, b = 5),hjust = 0.5)
    )
  )+
  ggsave("bop.png",width = 14, height = 10,dpi = 600)

# exchange rates data
er <- 
  read_csv("exchange_rates.csv") %>% 
  janitor::clean_names() %>% 
  mutate(date = dmy(observation_date),
         value = as.numeric(observation_value)) %>% 
  filter(series_name=="Average Exchange rate of Pak Rupees per U.S. Dollar",
         date >= ymd("1995-7-1")) %>% 
  select(date,er = value)

er_p1 <- 
  er %>% 
  ggplot()+
  geom_line(aes(x = date, y = er),color = "brown",linewidth = 0.9)+
  theme_bw()+
  labs(x = NULL, y = NULL,title = "USD/PKR over the years")+
  scale_y_continuous(breaks = scales::breaks_width(50))+
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y")+
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

er_p2 <- 
  er %>% 
  arrange(date) %>% 
  mutate(er = ((er/dplyr::lag(er,1))-1)) %>% 
  ggplot()+
  geom_col(aes(x = date, y = er),fill = "brown")+
  theme_bw()+
  labs(x = NULL, y = NULL,title = "Month-on-Month change in USD/PKR")+
  scale_y_continuous(
    breaks = scales::breaks_width(0.05),
    labels = scales::label_percent())+
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y")

  
(er_p1/er_p2) +
  plot_annotation(
    title = "Pakistan's Exchange Rate",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold",hjust = 0.5,margin = margin(t = 10)),
    )
  )+
  ggsave("er.png",height = 9, width = 14,dpi = 600)
