import byte_formatting

messages = ["TX fifo Full",
            "RX ready PipeNum",
            "Maximal Retransmits reached",
            "TX Data Sent",
            "RX Data Ready",
            "Register Bank"]
bitfield_format = [[1], [3], [1], [1], [1], [1]]

def main():
    inputnum = 14
    liste = byte_formatting.format_int_to_list(inputnum, bitfield_format)
    print("Status: %d means" % inputnum)
    for i in range(len(bitfield_format)):
        print("%s : %s" % (messages[i], str(liste[i])))

        
if __name__ == "__main__":
    main()
