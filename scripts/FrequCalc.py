
#fclk = 64000000
fclk = 144e6
frequ = 1000000

for presc in range(14):
    print("Timer Value: %f , Prescaler %d" %
          ((fclk/((1 << presc) * frequ)), presc))
