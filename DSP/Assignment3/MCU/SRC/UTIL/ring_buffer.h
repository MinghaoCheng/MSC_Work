#ifndef _RING_BUFFER_
#define _RING_BUFFER_

#include <stdint.h>
#include <stdbool.h>

typedef struct
{
  uint16_t next_in;
  uint16_t next_out;
  uint8_t *data;
  uint16_t capacity;
} ring_buffer_typedef;

void Init_ring_buffer(ring_buffer_typedef *ring_buffer, uint8_t *pBuffer, uint16_t size);
void Write_ring_buffer_byte(ring_buffer_typedef *ring_buffer, uint8_t data_in);
void Write_ring_buffer(ring_buffer_typedef *ring_buffer, uint8_t *pData_in, uint16_t data_size);
uint16_t Read_ring_buffer(ring_buffer_typedef *ring_buffer, uint8_t *pData_out, uint16_t read_size);
bool Is_ring_buffer_empty(ring_buffer_typedef *ring_buffer);

#endif
