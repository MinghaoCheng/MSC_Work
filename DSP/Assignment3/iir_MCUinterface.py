import serial
import threading
import collections
import struct
import time

class MCU_interface:
    def __init__(self, Port_name, Baudrate):
        self.__port = serial.Serial(port = Port_name, baudrate = Baudrate, bytesize = serial.EIGHTBITS,
                                stopbits = serial.STOPBITS_ONE)
        self.__polling_task_thread = threading.Thread(target = self.__polling_task, name = "serial_polling_thread")
        self.__que = collections.deque(maxlen = 64)
        self.__polling_task_thread.start()
    
    def __polling_task(self):
        self.__packet_lost = 0
        self.__frame_counter = 0
        while (True):
            # packet header
            # time.sleep(0.001)
            if(struct.unpack("B", self.__port.read(1))[0] == 115):
                # frame counter 0 ~ 255
                MCU_frame_counter = struct.unpack("B", self.__port.read(1))[0]
                # calculate packet lost
                if(MCU_frame_counter != self.__frame_counter):
                    self.__frame_counter = MCU_frame_counter
                    if(self.__packet_lost != 0):
                        self.__packet_lost += MCU_frame_counter - self.__frame_counter
                else:
                    self.__frame_counter += 1
                    if (self.__frame_counter == 256):
                        self.__frame_counter = 0
                # read payload
                MCU_frame_payload = struct.unpack("ssss", self.__port.read(4))
                voltage = MCU_frame_payload[0] + MCU_frame_payload[1] + MCU_frame_payload[2] + MCU_frame_payload[3]
                self.__que.append(int(voltage))

    def Is_buffer_empty(self):
        return (self.__que.__len__() == 0)

    def Read_one_packet(self):
        return self.__que.popleft()

    def Packet_lost(self):
        return self.__packet_lost
                


