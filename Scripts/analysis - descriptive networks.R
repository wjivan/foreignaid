# Fundamental packages
library(here)
library(cowplot)
library(tidyverse)
library(rlist)
library(gridExtra)

# Network
library(igraph)

# Network packages
library(tidygraph)
library(ggraph)
library(graphlayouts)

# https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
# Define color palette for colorblind (me!)
cbp2 <- c("#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#D55E00")

# Import data
panel_df <- read_rds(here('Input/panel_data.rds')) 

# Number of donors over time
recipient_df <- panel_df %>% 
  filter(aid_money>0) %>% 
  group_by(recipient, year) %>% 
  mutate(sum_received = sum(aid_money)) %>% 
  distinct(recipient, year, sum_received) %>% 
  group_by(year) %>% 
  mutate(total_count = n()) %>% 
  group_by(year) %>% 
  arrange(year, desc(sum_received)) %>% 
  top_n(n=5, wt=sum_received) %>% 
  mutate(top_countries = paste0(recipient, collapse=", ")) %>% 
  distinct(year, total_count, top_countries)

png(here("Output/recipient_table.png"))
grid.table(recipient_df)
dev.off()

# aid money over time
t <- panel_df %>% filter(aid_money>0) %>% group_by(year) %>% summarise(aid = sum(aid_money))

# Amount represented by the top n donors
dist_df <- panel_df %>% 
  group_by(donor, year) %>% 
  summarise(total_aid = sum(aid_money)) %>% 
  group_by(year) %>% 
  mutate(year_total =sum(total_aid)) %>% 
  ungroup() %>% 
  mutate(perc = total_aid/year_total) %>% 
  arrange(year, desc(perc)) %>% 
  group_by(year) %>% 
  mutate(cummulative = cumsum(perc)) %>% 
  filter(row_number() == 5)
  # On average, the top 5 countries represent around 90% of all the aid 

png(here("Output/donor_table.png"))
grid.table(recipient_df)
dev.off()

# Median and average amount
median(panel_df %>% select(aid_money) %>% filter(aid_money>0) %>% pull())*1000000
mean(panel_df %>% select(aid_money)  %>% filter(aid_money>0) %>% pull())*1000000
max(panel_df %>% select(aid_money)  %>% pull())*1000000
min(panel_df %>% select(aid_money)  %>% pull())*1000000
panel_df %>% filter(aid_money == max(aid_money))


# boxplot for donor
panel_df %>% select(donor, year, aid_money) %>% filter(aid_money>0) %>%
  arrange(year, aid_money) %>% 
  ggplot(aes(x=year, y=log(aid_money), group=year))+
  geom_boxplot()


# Number of recipients 
donor_df <- panel_df %>% 
  filter(aid_money>0) %>% 
  group_by(donor, year) %>% 
  mutate(sum_donated = sum(aid_money)) %>% 
  distinct(donor, year, sum_donated) %>% 
  group_by(year) %>% 
  mutate(total_count = n()) %>% 
  group_by(year) %>% 
  arrange(year, desc(sum_donated)) %>% 
  top_n(n=5, wt=sum_donated) %>% 
  mutate(top_countries = paste0(donor, collapse=", ")) 

donor_output <- donor_df %>% 
  distinct(year, total_count, top_countries)

# Top 10 recipients in 2019 and their donor countries

draw_network <- function(df, cty){
  
  if(cty==""){
    donor_network <- df %>% 
      filter(aid_money>0, year ==2018) %>% 
      group_by(recipient, year) %>% 
      mutate(sum_received = sum(aid_money)) %>% 
      select(donor, recipient, year, aid_money) %>% 
      ungroup()
  }else{
    donor_network <- df %>% 
      filter(aid_money>0, year ==2018, recipient==cty) %>% 
      group_by(recipient, year) %>% 
      mutate(sum_received = sum(aid_money)) %>% 
      select(donor, recipient, year, aid_money) %>% 
      ungroup()
  }
  
  
  node_df <- donor_network %>% 
    distinct(donor) %>% 
    rename(label = donor) %>% 
    full_join(donor_network %>% 
                distinct(recipient) %>% 
                rename(label = recipient), on = 'label') %>% 
    rowid_to_column("id")%>% 
    select(label, id)
  
  edges_df <- donor_network %>% 
    left_join(node_df, by=c("donor"="label")) %>% 
    rename(from = id) %>% 
    left_join(node_df, by=c("recipient"= "label")) %>% 
    rename(to = id) %>% 
    rename(weight = aid_money) %>% 
    select(from, to, weight)
  
  routes_tidy <- tbl_graph(nodes = node_df, edges = edges_df, directed = TRUE)%>% 
    # Activate the edges table and arrange by weight
    activate(edges) %>% 
    arrange(desc(weight))
  
  ggraph(routes_tidy, layout = "stress") + 
    geom_node_point() +
    geom_edge_link(alpha = 0.8,
                   arrow = arrow(length = unit(1, 'mm')),
                   end_cap = circle(3, 'mm'),
                   show.legend=FALSE) + 
    scale_edge_width(range = c(0.5, 2)) +
    geom_node_text(aes(label = label), repel = TRUE) +
    ggtitle(cty)+
    theme_graph()
  
}

countries <- panel_df %>%
  select(donor, recipient, year, aid_money) %>% 
  filter(aid_money>0, year ==2018) %>% 
  group_by(recipient) %>% 
  summarise(sum_received = sum(aid_money)) %>% 
  arrange(sum_received) %>% 
  slice_max(n=10, order_by=sum_received)%>% 
  distinct(recipient) %>% 
  pull()

plots = list()
for(c in countries[1:5]){
  p <- draw_network(panel_df, c)
  plots = list.append(plots, p)
}

png(here("Output/significant_recipients.png"), width=800, height=600)
plot_grid(plotlist=plots)
dev.off()


# Visualising network in 2018
# Subset the chart to only the top 5 donor countries
donor_list <- donor_df %>% 
  ungroup() %>% 
  filter(year == 2018) %>% 
  select(donor) %>% 
  pull()

donor_network <- panel_df %>% 
  filter(year==2018, donor %in% donor_list, aid_money>0) %>% 
  group_by(recipient, year) %>% 
  mutate(sum_received = sum(aid_money)) %>% 
  select(donor, recipient, year, aid_money) %>% 
  ungroup()

# find the corresponding major donor for the recipient
top_donor <- donor_network %>% 
  #add weighted degree
  group_by(recipient, year) %>% 
  mutate(aid_deg = aid_money/sum(aid_money)) %>% 
  ungroup() %>% 
  mutate_at(vars(aid_deg), ~replace_na(.,0)) %>% 
  group_by(recipient) %>% 
  mutate(dominant = max(aid_deg)) %>% 
  filter(dominant == aid_deg) %>% 
  rename(dominant_donor = donor)

# custom chart to visualise dominant donor
x_count <- 5

p <- top_donor %>%
  group_by(dominant_donor) %>%
  mutate(counts = n()) %>%
  arrange(desc(counts)) %>%
  mutate(
    x = rep(1:x_count, length.out = n()),
    y = rep(20:1, each = x_count, length.out = n())
  ) %>%
  ggplot() +
  geom_point(
    aes(
      x = x,
      y = y,
      size = dominant,
      fill = fct_reorder(dominant_donor, counts, .desc = TRUE)
    ),
    shape = 21,
    show.legend = T
  ) +
  scale_size(range = c(1, 6)) +
  #offset on the y scale
  scale_y_continuous(expand = c(0.2, 0)) +
  scale_x_continuous(expand = c(0, 0.2)) +
  geom_text(aes(x = x, y = y, label = recipient), position = position_nudge(y =
                                                                              0.3)) +
  facet_grid(
    vars(fct_reorder(dominant_donor, counts, .desc = TRUE)),
    scales = "free_y",
    space = "free",
    labeller = label_wrap_gen(width = 2, multi_line = TRUE)
  ) +
  guides(
    fill = guide_legend(override.aes = list(size = 8), title = "Top 5 donor countries"),
    size = guide_legend(title = "% of aids received")
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(colour = 'white', face = "bold")
  ) +
  scale_fill_manual(values = cbp2)

png(here("Output/recipients_group.png"), width=600, height=800)
plot_grid(p)
dev.off()

# set up nodes
node_df <- donor_network %>% 
  distinct(donor) %>% 
  rename(label = donor) %>% 
  full_join(donor_network %>% 
              distinct(recipient) %>% 
              rename(label = recipient), on = 'label') %>% 
  rowid_to_column("id")%>% 
  select(label, id)

edges_df <- donor_network %>% 
  left_join(node_df, by=c("donor"="label")) %>% 
  rename(from = id) %>% 
  left_join(node_df, by=c("recipient"= "label")) %>% 
  rename(to = id) %>% 
  rename(weight = aid_money) %>% 
  select(from, to, weight)

routes_tidy <- tbl_graph(nodes = node_df, edges = edges_df, directed = T)%>% 
  # Activate the edges table and arrange by weight
  activate(edges) %>% 
  arrange(desc(weight)) %>% 
  activate(nodes) %>% 
  left_join(top_donor %>% 
              select(label=recipient, dominant_donor)) %>% 
  mutate(dominant_donor = ifelse(is.na(dominant_donor), label, dominant_donor)) %>% 
  mutate(centrality = centrality_degree())


# # Use a manual approach
# # http://mr.schochastics.net/netVizR.html
# bb <- layout_as_backbone(routes_tidy,keep = 0.8)
# E(routes_tidy)$col <- FALSE
# E(routes_tidy)$col[bb$backbone] <- TRUE
# 
# ggraph(routes_tidy,layout="manual",x=bb$xy[,1],y=bb$xy[,2])+
#   geom_edge_link0(aes(col=col),width=0.1)+
#   geom_node_point()+
#   scale_color_brewer(palette = "Set1")+
#   scale_edge_color_manual(values=c(rgb(0,0,0,0.3),rgb(0,0,0,1)))+
#   theme_graph()+
#   theme(legend.position = "none")


p <- ggraph(routes_tidy,layout="dh") + 
  # ! Got lazy here and manually input the countries in order
  geom_node_point(aes(size=centrality, fill=factor(dominant_donor, levels=c("Germany","Japan","France","Norway","United Kingdom"))), 
                  shape=21,
                  show.legend = T) +
  geom_edge_link(alpha = 0.5,
                 arrow = arrow(length = unit(1, 'mm')),
                 end_cap = circle(3, 'mm'),
                 show.legend=FALSE,
                 edge_colour = "grey66") + 
  scale_size(range=c(3,10), guide="none")+
  geom_node_text(aes(label = label, size=centrality), 
                 repel = TRUE,
                 show.legend = F) +
  guides(fill = guide_legend(override.aes = list(size=8), title="Top 5 donor countries"))+
  scale_fill_manual(values = cbp2)+
  theme_graph()

png(here("Output/top5_network.png"), width=800, height=600)
plot_grid(p)
dev.off()
