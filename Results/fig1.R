library(dplyr)
library(ggplot2)
df1 = read.csv("articles.csv", stringsAsFactors = FALSE) %>% select(violent_ratio, topic)
names(df1)[2] = "article"

df2 = read.csv("debates.csv", stringsAsFactors = FALSE) %>% select(violent_ratio, debate)
df2$debate = paste0("Debate ",df2$debate)
names(df2)[2] = "article"

df = rbind(df1, df2)
df$article = as.factor(df$article)

df$article = factor(df$article, levels = c("Debate 1","Debate 2", "Debate 3", "Hurricane Sandy", "Syrian War","Drought", "Nobel Peace Prize", "General Election"))
df$violent_ratio = df$violent_ratio * 100

df = filter(df, article != "General Election") %>% droplevels()

fmt <- function(){
    f <- function(x) as.character(round(x,2))
    f
}


gp = ggplot(df, aes(x = 1, y = violent_ratio)) + 
    geom_point(size = 3, 
               position = position_jitter(width = .1, height = 0)) 
gp = gp  + 
    geom_errorbar(stat = "hline", yintercept = "mean",
                  aes(ymax=..y..,ymin=..y..),
                  width = 0.5, col = "black") +
    facet_grid(~article ) +
    ylab("Percentage of violent words\n") +
    scale_x_continuous(breaks=NULL, name="")+ 
    scale_y_continuous(breaks=seq(0,5),labels = format(c(0,1,2,3,4,5), digits = 2, nsmall = 2))
    # theme_grey(base_size = 20)

gp = gp + theme(axis.text.y = element_text(size=18),
           axis.title.y = element_text(size=22),
           strip.text.x = element_text(size=12.3) )


ggsave(gp, file="fig1.png", width=12, height=6)
ggsave(gp, file="fig1.eps", width=12, height=6)

gp 
