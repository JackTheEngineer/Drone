import sys
import serial


meanings = { 0x1:"SLAVE_SELECT",
             0x2:"WRONG_TDF_CODE_FOUND",
             0x4:"START_CONDITION_RECEIVED",
             0x8:"REPEATED_START_CONDITION_RECEIVED",
             0x10:"STOP_CONDITION_RECEIVED",
             0x20:"NACK_RECEIVED",
             0x40:"ARBITRATION_LOST",
             0x80:"SLAVE_READ_REQUESTED",
             0x100:"ERROR",
             0x200:"ACK_RECEIVED",
             0x400:"RECEIVER_START_INDICATION",
             0x800:"DATA_LOST_INDICATION",
             0x1000:"TRANSMIT_SHIFT_INDICATION",
             0x2000:"TRANSMIT_BUFFER_INDICATION",
             0x4000:"RECEIVE_INDICATION",
             0x8000:"ALTERNATIVE_RECEIVE_INDICATION",
             0x10000:"BAUD_RATE_GENERATOR_INDICATION",}

def print_meanings(num):
    for elem in meanings.keys():
        if((elem & num) > 0):
            print(meanings[elem])

def main():
    sp = serial.Serial("/dev/ttyUSB0",
                       57600,
                       serial.EIGHTBITS,
                       serial.PARITY_NONE,
                       serial.STOPBITS_ONE)
    sp.close()
    # sp.open()
    # while(True):
    #     string = sp.read(20)
    #     print(string)

    num = 13652 #5460
    print_meanings(num)

if __name__ == "__main__":
    main()
