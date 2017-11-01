# Raw data directory
dir = '/global/scratch/paciorek/wikistats_full'

# Eead data and do some checks 
lines = sc.textFile(dir + '/' + 'dated')
lines.getNumPartitions() # 16590 (480 input files) for full dataset

# Check the number of lines
lines.count() # 9467817626 for full dataset

# watch the UI and watch wwall as computation progresses
testLines = lines.take(10)
testLines[0]
testLines[9]

# Filtering and processing functions
# Importations
import re
from operator import add

# Find the expression
def find(line, regex = "Chile", language = None):
	vals = line.split(' ')
	if len(vals) < 6:
		return(False)
	tmp = re.search(regex, vals[3])
	if tmp is None or (language != None and vals[2] != language):
		return(False)
	else:
		return(True)

lines.filter(find).take(100) # pretty quick

# not clear if should repartition; will likely have small partitions if not
chile = lines.filter(find).repartition(480) # 18 minutes for full
chile.count() # 433k observations for full dataset

### map-reduce step to sum hits across date-time-language triplets###
def stratify(line):
	# create key-value pairs where:
	# key = date-time-language
	# value = number of website hits
	vals = line.split(' ')
	return(vals[0] + '-' + vals[1] + '-' + vals[2], int(vals[4]))

# sum number of hits for each date-time-language value
counts = chile.map(stratify).reduceByKey(add) # 5 minutes
# 128889 for full dataset

# Map step to prepare output 
def transform(vals):
# split key info back into separate fields
	key = vals[0].split('-')
	return(",".join((key[0], key[1], key[2], str(vals[1]))))

### output to file ###
# have one partition because one file p
outputDir = '/global/home/users/cpaismz/ChileFull'
counts.map(transform).repartition(1).saveAsTextFile(outputDir) 