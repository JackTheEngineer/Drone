import serial
import byte_formatting

messages = ["RX Empty Flag",
            "RX Full Flag",
            "Reserved",
            "TX Empty Flag",
            "TX Full Flag",
            "TX Reuse",
            "Reserved",
            "TX fifo Full",
            "RX ready PipeNum",
            "Maximal Retransmits reached",
            "TX Data Sent",
            "RX Data Ready",
            "Register Bank",
            ]
bitfield_format = [[1], [1], [2], [1], [1], [1], [1],
                   [1], [3], [1], [1], [1], [1]]

messages = ['J0', 'J1', 'J2', 'J3']
joystick_bitfieldformat = [[16],[16],[16],[16]]

def main():
    ser_port = serial.Serial('/dev/ttyUSB3', 460800)

    while(True):
        byt = ser_port.readline()
        vals = byt[:-1]
        integer = byte_formatting._8ui8_to_ui64(vals)
        values = byte_formatting.format_int_to_list(integer, joystick_bitfieldformat)
        print(values)
        # integer =  (vals[1] << 8 ) | vals[0]
        # liste = byte_formatting.format_int_to_list(integer, bitfield_format)
        # for i in range(len(bitfield_format)):
        #     print("%s : %s" % (messages[i], str(liste[i])))
        # print(end="\n")
        
    
    # sp.open()
    # while(True):
    #     string = sp.read(20)
    #     print(string)

if __name__ == "__main__":
      main()
            
