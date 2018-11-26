import iir_MCUinterface



def main():
    protocol_handler = iir_MCUinterface.MCU_interface("com3", 115200)

    while(True):
        if(protocol_handler.Is_buffer_empty() == False):
            temp = protocol_handler.Read_one_packet()
            print(temp)
        if(protocol_handler.Packet_lost() != 0):
            print("pakcet lost" + protocol_handler.Packet_lost())







if __name__ == "__main__":
    main()