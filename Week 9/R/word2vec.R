#
# Word2vec example, from http://www.rpubs.com/mukul13/rword2vec
#
library (rword2vec)
# Build model
# model <- word2vec (train_file = "text8", output_file = "vec.bin", binary = 1)
#
# Distances from random words, like "king"
#
distance (file_name = "vec.bin",search_word = "king", num = 10)
# ...or "sodium"
distance (file_name = "vec.bin",search_word = "sodium", num = 10)
# ...or "aunt"
distance (file_name = "vec.bin",search_word = "aunt", num = 10)
#... or "France"
distance (file_name = "vec.bin",search_word = "france", num = 10)
#
#
# Vocab file -- interesting just 'cause it is
#
# vocab_count ("text8", "vocab.egad")
# 253,854 words -- but check 'em out
#
# Word 2 phrase is supposed to work like this, I think, but...
#
# phr <- word2phrase(train_file = "text8",output_file = "vec.txt")
# word2vec ("vec.txt", output_file = "vec.bin.new", binary = 1)
#
distance (file_name = "vec.txt",search_word = "high_commisioner", num = 10)

# The analoga-liz-inator!
#
word_analogy ("vec.bin", c("king queen man"), num = 10) # not bad
word_analogy ("vec.bin", c("king queen uncle"), num = 10) # imperfect
word_analogy ("vec.bin", c("sodium chloride calcium"), num = 10) # 
word_analogy ("vec.bin", c("niece nephew sister"), num = 10) # 
word_analogy ("vec.bin", c("paris france berlin"), num = 10) # 
word_analogy ("vec.bin", c("france paris germany"), num = 10) # 
#
# Something harder
#
word_analogy ("vec.bin", c("england pound japan"), num = 10) # 
word_analogy ("vec.bin", c("joyce ireland kipling"), num = 10) # ?
word_analogy ("vec.bin", c("lincoln nebraska tasmania"), num = 10) # 
word_analogy ("vec.bin", c("lincoln nebraska albany"), num = 10) # 
word_analogy ("vec.bin", c("illinois chicago california"), num = 10) # 














