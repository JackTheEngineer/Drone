
fclk = 120000000
frequ = 50

for presc in range(14):
    print(fclk/((1<<presc) * frequ), end = " ")
    print(presc)
