
fclk = 64000000
frequ = 1000000

for presc in range(14):
    print("Timer Value: %d , Prescaler %d" % ((fclk/((1<<presc) * frequ)),presc))
