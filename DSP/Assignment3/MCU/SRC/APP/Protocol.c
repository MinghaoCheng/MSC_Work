#include "Protocol.h"
#include "Uart.h"

volatile uint8_t Protocol_frame_counter = 0;
uint8_t Packet_frame[6];

void Protocol_Init(void)
{
    UART_Enable_TX_DMA(Packet_frame);
}

void Protocol_send_packet(uint16_t payload)
{   
    Packet_frame[0] = 's';
    Packet_frame[1] = Protocol_frame_counter;
    Packet_frame[2] = (payload / 1000) + 0x30;
    Packet_frame[3] = ((payload / 100) % 10) + 0x30;
    Packet_frame[4] = ((payload / 10) % 10) + 0x30;
    Packet_frame[5] = (payload% 10) + 0x30;
    
    Protocol_frame_counter ++;
    
    UART_Send_Buffer_DMA(6);
}

uint8_t Protocol_get_frame_counter(void)
{
    return Protocol_frame_counter;
}