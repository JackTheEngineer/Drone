
def check_format_being_leq_64(formatt):
    summ = 0
    for i in range(len(formatt)):
        summ += formatt[i][1]

    if(summ <= 64):
        return True
    else:
        return False


def formatt_to_int_format(formatt):
    int_format = []
    for elem in formatt:
        if(len(elem) == 2):
            int_format.append([elem[1]])
        elif(len(elem) == 3):
            int_format.append([elem[1], elem[2]])
        elif(len(elem) == 4):
            int_format.append([elem[1], elem[2], elem[3]])
        else:
            raise ValueError("Length of format is neiter 2,3 nor 4")
    return int_format


def format_int_to_dict(bitfield, formatt):
    """
    @brief formats an intergerfield to a dictionary
           using a certain formatting list
    @param integerfield is the 32 bit or 64 bit variable
    @param formatt is the formatting array that used to format
    the correct value
    [[String, bitsize, (optional) offset, (optional) factor],
    ...
]
    """

    int_format = formatt_to_int_format(formatt)
    values = format_int_to_list(bitfield, int_format)
    msg_dict = {}

    for i in range(len(formatt)):
        msg_dict[formatt[i][0]] = values[i]
    return msg_dict


def format_dict_to_int(dictio, formatt):
    """ 
    @brief function to format a dictionary with values to 
           an integer using a certain format
    @param dictio has {String, value}
    @param format has {String, bitsize, offset, factor}
    """ 
    if(len(dictio) > len(formatt)):
        raise ValueError("The dictionary with the contents"
                         "%s"
                         "has more elements than the format"
                         "%s" % (str(dict), str(formatt)))
    format_names = [elem[0] for elem in formatt]

    int_format = formatt_to_int_format(formatt)
    values = []
    
    for elem in format_names:
        if(elem not in dictio):
            values.append(0)
        else:
            values.append(dictio[elem])

    return format_list_to_int(values, int_format)


def check_int_format_being_leq_64(int_format):
    summ = 0
    for elem in int_format:
        summ += elem[0]

    if(summ <= 64):
        return True
    else:
        return False

    
def check_int_format_bitsizes_bigger_than_1(int_format):
    for elem in int_format:
        if(elem[0] < 1):
            return False
    return True


def check_values_fit_into_bitrange(data, int_format):
    if(len(data) != len(int_format)):
        raise ValueError("The format %s and the data %s"
                         "don't have the same length"
                         % (str(int_format), str(data)))
    for i in range(len(data)):
        format_elem = get_full_format_element(int_format[i])
        maxnum_of_bitrange = (1 << format_elem[0]) - 1
        offset = format_elem[1]
        factor = format_elem[2]
        # Only Positive Number for the resulting transmitdata
        # are allowed
        transmitnum = (data[i] - offset)/factor
        if(transmitnum > maxnum_of_bitrange):
            return False
    return True


def get_full_format_element(format_element):
    # returns the full format_element
    # with the meanings
    # [ bitsize, offset, factor]
    if(len(format_element) == 1):
        return([format_element[0], 0, 1])
    elif(len(format_element) == 2):
        return([format_element[0],
                format_element[1],
                1])
    elif(len(format_element) == 3):
        return format_element
    else:
        raise ValueError("The length of the format "
                         "element %s is not in 1,2 or 3 "
                         % (str(format_element)))

    
def validate_int_format_raising_errors(int_format):
    if(not check_int_format_bitsizes_bigger_than_1(int_format)):
        raise ValueError("The format %s has bitsizes"
                         "smaller than 1"
                         % (str(int_format)))
    if(not check_int_format_being_leq_64(int_format)):
        raise ValueError("The format %s has more than "
                         "64 bits. This is not allowed"
                         % (str(int_format)))


def validate_data_and_format_raising_errors(data, int_format):
    validate_int_format_raising_errors(int_format)
    if(len(data) != len(int_format)):
        raise ValueError("The format %s and the data %s"
                         "don't have the same length"
                         % (str(int_format), str(data)))
    if(not check_values_fit_into_bitrange(data, int_format)):
        raise ValueError("The transmitvalues with "
                         "transmitval = (val - offset)/factor "
                         "with the data"
                         "%s "
                         "do not fit into the bitrange "
                         "of format %s " % (str(data), str(int_format)))


def format_list_to_int(liste, int_format, transmit=True):
    # liste of the form
    # [ number, number, number, number ]
    # which should be transmitted
    #
    # takes int format of the form
    # [ [bitsize, (optional)offset, (optional)factor]
    #   [bitsize, (optional)offset, (optional)factor]...]
    #
    # 'transmit' argument defaults to true,
    # because this is mainly used for transmitting
    #

    bitfield = 0
    shiftsum = 0

    validate_data_and_format_raising_errors(liste, int_format)
    for i in range(len(liste)):
        full_format = get_full_format_element(int_format[i])
        bitfieldsize = full_format[0]
        offset = full_format[1]
        factor = full_format[2]

        if(not transmit):
            offset = -offset
            factor = 1/factor

        stringmask = "0b" + (bitfieldsize * "1")
        bitmask = int(stringmask, 2)

        transmitvalue = (int)((liste[i] - offset)/factor) & bitmask

        bitfield = bitfield | (transmitvalue << shiftsum)

        shiftsum += bitfieldsize

    return bitfield


def format_int_to_list(bitfield, int_format, receive=True):
    shiftsum = 0
    values = []
    validate_int_format_raising_errors(int_format)
    for i in range(len(int_format)):
        full_format = get_full_format_element(int_format[i])
        bitfieldsize = full_format[0]
        offset = full_format[1]
        factor = full_format[2]

        if(not receive):
            offset = -offset
            factor = 1/factor

        stringmask = "0b" + (bitfieldsize * "1")
        bitmask = int(stringmask, 2)

        receivevalue = ((bitfield >> shiftsum) & bitmask)
        shiftsum += bitfieldsize
        values.append((receivevalue + offset) * factor)

    return values


def ui64_to_8ui8(bitfield):
    message = []
    for j in range(8):
        message.append((bitfield >> (j*8)) & 0xFF)
    return message


def _8ui8_to_ui64(message):
    i = 0
    for j in range(len(message)):
        i = i | (message[j] << (j*8))
    return i
