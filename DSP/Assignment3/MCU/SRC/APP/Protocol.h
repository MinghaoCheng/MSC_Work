#ifndef _PROTOCOL_H_
#define _PROTOCOL_H_

#include <stdint.h>

void Protocol_Init(void);
void Protocol_send_packet(uint16_t payload);
uint8_t Protocol_get_frame_counter(void);


#endif
