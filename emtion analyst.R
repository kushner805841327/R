# 载入sentiment 数据集的情感词典
library(tidytext)
sentiments

#tidy还提供了get_sentiments()函数
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

#内连接的情感分析
library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text,regex("^chapter[\\divxlc]",ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word,text)

tidy_books

#为方便起变，这里将unnest_tokens()函数输出的列命名为word,因为情感词典和停止词数据集的列向量
nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")
tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrcjoy) %>%
  count(word,sort = TRUE)

# 文本从何处开始，使用spread（）方法对各列进行负面情感和正面情感计算，最后计算尽情感分数
library(tidyr)

janeaustensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book,index = linenumber %/% 80,sentiment) %>%
  spread(sentiment,n,fill = 0) %>%
  mutate(sentiment = positive - negative)

library(ggplot2)
ggplot(janeaustensentiment,aes(index,sentiment,fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book,ncol = 2,scales = "free_x")

#比较三个情感词典
pride_prejudice <- tidy_books %>%
  filter(book == "Pride & Prejudice")

pride_prejudice

#采用整除法（%/%）来定义跨行更多的大章节文本
#可以使用与count(),spread()和mutate()相同的模式来查找这些节中的每行文本的净情感值

afinn <- pride_prejudice %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  pride_prejudice %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  pride_prejudice %>%
    inner_join(get_sentiments("nrc")) %>%
    filter(sentiment %in% c("positive","negative"))) %>%
  mutate(method = "NRC") %>%
  count(method,index = linenumber %/% 80,sentiment) %>%
  spread(sentiment,n,fill = 0) %>%
  mutate(sentiment = positive - negative)
    
#通过???每个情感词典的净值估计小说的每块文本，并把这些估计综合起来

bind_rows(afinn,
          bing_and_nrc) %>%
  ggplot(aes(index,sentiment,fill = method)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~method,ncol = 1,scales = "free_y")

#词典正面单词和负面单词的分布
get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive",
                          "negative")) %>%
  count(sentiment)

get_sentiments("bing") %>%
  count(sentiment)

#最常见的正面单词和负面单词
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort = TRUE) %>%
  ungroup()

bing_word_counts

#可视化展现，并将数据源作为整洁文本处理
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word,n,fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y="Contribution to sentiment",
       x= NULL)+
  coord_flip()


#手工添加停用词——bind_rows
custom_stop_words <- bind_rows(data_frame(word=c("miss"),
                                         lexicon = c("custom")),
                              stop_words)

custom_stop_words

#Wordclouds模块

library(wordcloud)
tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word,n,max.words = 100))

#结合正负性，基于comparison.cloud呈现正负性词云
library(reshape2)

tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment,value.var = "n",fill= 0) %>%
  comparison.cloud(colors = c("gray20","gray80"),max.words = 100 )




  
  
  
  
  