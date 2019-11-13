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
size = 16
joystick_bitfieldformat = [[size],[size],[size],[size]]

def main():
    ser_port = serial.Serial('/dev/ttyACM0', 115200)

    while(True):
        byt = ser_port.readline()
        print(byt)
        # vals = byt
        # integer = byte_formatting._8ui8_to_ui64(vals[:-1])
        # values = byte_formatting.format_int_to_list(integer, joystick_bitfieldformat)
        # print(values)

if __name__ == "__main__":
      main()
            
