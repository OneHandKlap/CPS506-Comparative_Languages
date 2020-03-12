import math


def cancer(n):
    if n==1:
        return 1
    else:
       return (math.floor(n/2)*(math.ceil(n/2)))+cancer(math.floor(n/2))+cancer(math.ceil(n/2))

print(cancer(13))