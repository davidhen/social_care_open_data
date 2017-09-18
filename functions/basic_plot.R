# Basic plot function for descriptives

basic_plot <- function(x){
  
  plot <- 
    x %>%
    arrange(-cnt) %>%
    mutate(data = factor(data, data)) %>%
    ggplot() +
    geom_col(aes(x = data, y = cnt)) + 
    geom_text(data = x, aes(label=paste0(round((frq * 100), 1),"%"), x= data,
                            y= cnt), size=3.5, vjust = -0.5, colour = "black") +
    theme(axis.text.x = element_text(angle = 45, size = 10, hjust = 1, vjust = 1)) +
    theme_hc()
  plot
}