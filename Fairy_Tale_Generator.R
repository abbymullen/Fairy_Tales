collect_directory_into_text_frame = function(dirname) {
  text_files = list.files(dirname,full.names = TRUE)
  readSOTU = function(file) {
    message(file)
    text = scan(file,sep="\n",what="raw")
    words = text %>% 
      strsplit("[^A-Za-z]") %>% 
      unlist
    SOTU = data.frame(word=words,stringsAsFactors = FALSE)  
    return(SOTU)
  }
  all = lapply(text_files,readSOTU)
  all_texts = rbind_all(all)
  return(all_texts)
}
word_list = collect_directory_into_text_frame("Fairy_Tales/")

fairytale = word_list %<>% as.tbl %>% filter(word!="")

fairytale %<>% mutate(word2=lead(word,1),word3=lead(word,2))  

transitions = fairytale %>% 
  group_by(word) %>% 
  mutate(word1Count=n()) %>% 
  group_by(word,word2) %>% 
  mutate(chance = n()/word1Count,bigramCount=n()) %>% 
  group_by(word,word2,word3) %>% 
  summarize(trigramChance = n()/bigramCount[1])

transitions = transitions[complete.cases(transitions), ]

findNextWord = function(previous,current) {
  subset = transitions %>% 
    filter(word==previous,word2==current) 
  nextWord = sample(subset$word3,1,prob = subset$trigramChance)
  return(nextWord)
}

previous = "once"
current = "upon"
while (TRUE) {
  message(previous)
  nextWord = findNextWord(previous,current)
  previous = current
  current = nextWord
}