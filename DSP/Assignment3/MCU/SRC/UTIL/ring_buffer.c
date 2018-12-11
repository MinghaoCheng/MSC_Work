#include "ring_buffer.h"
void Init_ring_buffer(ring_buffer_typedef *ring_buffer, uint8_t *pBuffer, uint16_t size)
{
  ring_buffer->data = pBuffer;
  ring_buffer->capacity = size;
  ring_buffer->next_in = 0;
  ring_buffer->next_out = 0;
}
void Write_ring_buffer_byte(ring_buffer_typedef *ring_buffer, uint8_t data_in)
{
  ring_buffer->data[ring_buffer->next_in++] = data_in;
  if (ring_buffer->next_in >= ring_buffer->capacity)
    ring_buffer->next_in = 0;
}
void Write_ring_buffer(ring_buffer_typedef *ring_buffer, uint8_t *pData_in, uint16_t data_size)
{
  uint8_t i = 0;
  for (i = 0; i < data_size; i++)
  {
    ring_buffer->data[ring_buffer->next_in++] = pData_in[i];
    if (ring_buffer->next_in >= ring_buffer->capacity)
      ring_buffer->next_in = 0;
  }
}
uint16_t Read_ring_buffer(ring_buffer_typedef *ring_buffer, uint8_t *pData_out, uint16_t read_size)
{
  uint16_t i = 0;
  for (i = 0; i < read_size; i++)
  {
    if (ring_buffer->next_out != ring_buffer->next_in)
    {
      pData_out[i] = ring_buffer->data[ring_buffer->next_out++];
      if (ring_buffer->next_out >= ring_buffer->capacity)
        ring_buffer->next_out = 0;
    }
    else
    {
      break;
    }
  }
  return i;
}
bool Is_ring_buffer_empty(ring_buffer_typedef *ring_buffer)
{
  if(ring_buffer->next_in == ring_buffer->next_out)
    return true;
  return false;
}