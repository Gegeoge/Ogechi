library(quanteda)
library(readtext)

filelist = list.files('/Users/gege/Desktop/Legal Analytics Research Project/StateModelAdoption_IndividualStatesCSVfiles', full.names = TRUE, pattern = "*.csv")

document_list = c()
for (file in filelist) {
  df=read.csv(file)
  df_allbody = paste(df[,'Body'],collapse = " ")
  df_allbody = as.character(df_allbody)
  state = tail(strsplit(file, "/")[[1]], 1)
  document_list[state] = df_allbody
}

allsections = readtext("/Users/Desktop/Legal Analytics /StateModelAdoption_IndividualStatesCSVfiles/All Sections.txt")
document_list["All Sections.txt"]=as.character(allsections[1,'text'])

mycorpus = corpus(document_list, names(document_list))

# Convert your corpus into tokens
ins_toks <- tokens(mycorpus)
ins_dfm <- dfm(mycorpus, 
               remove = c(stopwords("en")),
               remove_punct = TRUE,
               remove_num = TRUE)



# Similarity between one doc and remaining docs in corpus
ins_sim <- textstat_simil(ins_dfm,
                          method = "cosine")
ins_sim["All Sections.txt",]


#ins_sim # generates similarity measure [higher number = more similar] 

# Export to csv if you like for further examination
write.table(ins_sim, "ins_simil1.csv")

# Similarity among all pairs of documents in corpus
#fdd_sim_all <- textstat_simil(ins_dfm, 
                              #margin = "documents", 
                              #method = "cosine")

#fdd_sim_all_matrix <- as.matrix(fdd_sim_all) # Convert to a matrix; cells = similarity scores

#View(fdd_sim_all_matrix)
#write.table(ins_sim, "ins_simil2.csv")

# Decide on a cutoff similarity score to call a pair a match [here, cutoff is 0.9, or 90% similarity]
#diag(fdd_sim_all_matrix) <- 0 # sets the diagonal to zero
#fdd_sim_all_matrix[fdd_sim_all_matrix >= .9] <- 1 # sets all scores at or above 0.9 as 1
#fdd_sim_all_matrix[fdd_sim_all_matrix < .9] <- 0 # below as 0

#View(fdd_sim_all_matrix)

