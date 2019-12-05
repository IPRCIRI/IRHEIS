rfiles <- dir(pattern = glob2rx("*.R"))
rfiles
for(f in rfiles[6:8])
  source(f)