# Setting the working directory
setwd("C:/Users/chile/Desktop/Stats243/HW/HW3/Code/Working/Functions/26Sep/")

# Loading libraries
library("stringr")

# Loading functions
source("ExtractUnlist.R")

## Summary per play
# Number of unique speakers: Extract unique speakers and count them
SpeakerIndexes <- lapply(CleanPlaysSep, function(x) seq(1, length(unlist(x)), 2))
UniqueSpeakers <- lapply(seq_along(CleanPlaysSep), 
                         function(x) 
                           unique(str_to_upper(ExtractUnlist(CleanPlaysSep, x, SpeakerIndexes)))
                         )
NUniqueSpeakers <- lapply(UniqueSpeakers, function(x) length(unlist(x)))

# Number of spoken chunks
NSpoken <- lapply(CleanPlaysSep, function(x) length(unlist(x)) / 2)

# Number of sentences
ChunkIndexes <- lapply(CleanPlaysSep, function(x) seq(2, length(unlist(x)), 2))
SentencePattern <- '[^\\.]*[\\.\\?!]'
Sentences <- lapply(seq(CleanPlaysSep),
                        function(x) {
                          unlist(
                            str_extract_all(
                              ExtractUnlist(CleanPlaysSep, x, ChunkIndexes), 
                              SentencePattern)
                          )
                        }
                    )
NSentences <- lapply(Sentences, function(x) length(unlist(x)))

# Number of words (all)
WordsPattern <- '\\b[A-Za-z\']+\\b'
Words <- lapply(seq(CleanPlaysSep),
                    function(x){
                      unlist(
                        str_extract_all(
                          ExtractUnlist(CleanPlaysSep, x, ChunkIndexes),
                          WordsPattern)
                      )
                    }
)
NWords <- lapply(Words, function(x) length(unlist(x)))

# Average number of words per chunk: 3 decimals
AverageWordsChunk <- lapply(seq(NWords), function(x) unlist(NWords[x])/unlist(NSpoken[x]))
AverageWordsChunk <- gsub('(\\d+\\.\\d{3}).*', '\\1', AverageWordsChunk)

# Unique words (no repetitions)
UniqueWords <- lapply(Words, function(x) unique(str_to_lower(unlist(x))))
NUniqueWords <- lapply(UniqueWords, function(x) length(unlist(x)))


## Display results (summary)
# Unique Speakers
UniqueSpeakers
unlist(NUniqueSpeakers)

# Number of chunks
unlist(NSpoken)

# Number of sentences
unlist(NSentences)

# Number of words (all)
unlist(NWords)

# Average number of words per chunk: 3 decimals
AverageWordsChunk

# Unique words (no repetitions)
unlist(NUniqueWords)



# Save all the information to a .txt file
ToSummary <- list(UniqueSpeakers, unlist(NUniqueSpeakers), unlist(NSpoken),
                  unlist(NSentences), unlist(NWords), AverageWordsChunk,
                  unlist(NUniqueWords))
Separators <- c('--------------------UniqueSpeakers-------------------',
                '--------------Number of unique Speakers--------------',
                '---------------Number of Spoken Chunks---------------',
                '---------------.-Number of sentences-.---------------',
                '----------------Number of Words (Total)--------------',
                '----------Average number of words per chunk----------',
                '-----------------Number of Unique Words--------------')

# Create the txt file (loop over separators and information)
for (n in 1:length(ToSummary)){
  write(Separators[n], "Summary.txt", append = TRUE)
  write(unlist(ToSummary[n]), "Summary.txt", append = TRUE)  
}

