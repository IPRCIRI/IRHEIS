rfiles <- dir(pattern = glob2rx("*.R"))
for(f in rfiles[12:21])
  source(f)