library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text,regex("^chapter[\\divxlc]",
                                                ignore_case = TRUE)))) %>%
  ungroup()

library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word,text)

data(stop_words)
tidy_books <- tidy_books %>%
  anti_join(stop_words)

tidy_books %>%
  count(word,sort = TRUE)

library(ggplot2)

tidy_books %>%
  count(word,sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

library(gutenbergr)# just use the Janeaustenr do the reasech

hgwells <- gutenberg_download(c(35,36,5230,159))

tidy_hgwells <- hgwells %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words)

tidy_hgwells %>%
  count(word,sort = TRUE)

bronte <-gutenberg_download(c(1260,768,969,767))
tidy_bronte <-bronte %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words)

# chanjian
tidy_bronte %>%
  count(word,sort = TRUE)


# 绘图及比较不同小说在关键词上的数据框

library(tidyr)
frequency <- bind_rows(mutate(tidy_bronte,author = "Brontё Sister"),
                       mutate(tidy_hgwells,author = "H.G.Wells"),
                       mutate(tidy_books,author = "Jane Austen")) %>%
  mutate(word = str_extract(word,"[a-z']+")) %>%
  count(author,word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author,proportion) %>%
  gather(author,proportion,'Brontё Sister':'H.G.Wells')

library(scales)
library(ggplot2)
#expect a warning about rows with missing  values being removed 
ggplot(frequency,aes(x = proportion,y = 'Jane Austen',
                     color = abs('Jane Austen' - proportion))) +
  geom_abline(color = "gray40",lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5,width = 0.3,height = 0.3) +
  geom_text(aes(label = word),check_overlap =  TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) + 
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0,0.001),
                       low = "darkslategray4",high = "gray75") +
  facet_wrap(~author,ncol = 2) +
  theme(legend.position = "one") +
  labs(y = "Jane Austen", x = NULL)

#相关性测试来量化词频集的相似程度
cor.test(data = frequency[frequency$author == "Brontё Sister",],~ proportion + 'Jane Austen')








