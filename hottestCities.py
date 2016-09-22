# find the most commonly visited cities (American & not)
import operator

fin = open("ClickStreamData.new.txt","r")
tab = "\t"

# read first line for S&G's
null = fin.readline()

cityList = {}
cityList2 = {}

for line in fin:
	TS, IP, UR, UI, CI, RE, CO = line.split(tab)
	CO, null = CO.split("\n")
	CO, null = CO.split("\r")
	if not "United States" in CO:
	    if CI in cityList2:
	    	cityList2[CI] += 1
	    else:
	    	cityList2[CI] = 1
	else:
	    if CI in cityList:
	    	cityList[CI] += 1
	    else:
	    	cityList[CI] = 1
        
fin.close()

sortedCities = sorted(cityList.items(), key=operator.itemgetter(1), reverse=True)

for i in range(0,10):
	thisLine = sortedCities[i]
	print(thisLine)


sortedCities = sorted(cityList2.items(), key=operator.itemgetter(1), reverse=True)
for i in range(0,10):
	thisLine = sortedCities[i]
	print(thisLine)
