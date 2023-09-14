# install.packages("chatgpt")
library("chatgpt")
# Sys.setenv(OPENAI_API_KEY = "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") # koval.andrey
Sys.setenv(OPENAI_API_KEY = "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") # andriy.v.koval
cat(chatgpt::ask_chatgpt("I want to create a box plot using the ggplot2 package."))
 