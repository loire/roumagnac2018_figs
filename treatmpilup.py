import sys
name = sys.argv[1].split(".")[0]
with open(sys.argv[1],'r') as f:
    for line in f:
        data = line.split()
        pos = data[1]
        cov = data[3]
        if int(cov) > 0:
            match = data[4].count(",")
            match += data[4].count(".")
            sim = match/float(cov)
            print(name,pos,cov,match,sim*100)
        else:
            print(name,pos,cov,"NA","NA")

