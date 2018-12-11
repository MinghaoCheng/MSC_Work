#include "Uart.h"
#include "ring_buffer.h"

ring_buffer_typedef RX_ringbuffer;
uint8_t RX_buffer[256];
volatile bool RX_flag = false;

#define BUFSIZE 128
static char buff[BUFSIZE+16];

volatile bool Is_printf_enabled = true;

void UART_Init(void)
{
    LL_AHB1_GRP1_EnableClock(LL_AHB1_GRP1_PERIPH_GPIOA);
    LL_APB1_GRP1_EnableClock(LL_APB1_GRP1_PERIPH_USART2);

    /* Configure TX Pin */
    LL_GPIO_SetOutputPin(USARTX_TX_GPIO_PORT, USARTX_TX_PIN);
    LL_GPIO_SetPinMode(USARTX_TX_GPIO_PORT, USARTX_TX_PIN, LL_GPIO_MODE_ALTERNATE);
    LL_GPIO_SetAFPin_0_7(USARTX_TX_GPIO_PORT, USARTX_TX_PIN, USARTX_TX_AF);
    LL_GPIO_SetPinSpeed(USARTX_TX_GPIO_PORT, USARTX_TX_PIN, LL_GPIO_SPEED_FREQ_HIGH);
    LL_GPIO_SetPinOutputType(USARTX_TX_GPIO_PORT, USARTX_TX_PIN, LL_GPIO_OUTPUT_PUSHPULL);
    LL_GPIO_SetPinPull(USARTX_TX_GPIO_PORT, USARTX_TX_PIN, LL_GPIO_PULL_NO);

    /* Configure RX Pin*/
    LL_GPIO_SetPinMode(USARTX_RX_GPIO_PORT, USARTX_RX_PIN, LL_GPIO_MODE_ALTERNATE);
    LL_GPIO_SetAFPin_0_7(USARTX_RX_GPIO_PORT, USARTX_RX_PIN, USARTX_RX_AF);
    LL_GPIO_SetPinSpeed(USARTX_RX_GPIO_PORT, USARTX_RX_PIN, LL_GPIO_SPEED_FREQ_HIGH);
    LL_GPIO_SetPinPull(USARTX_RX_GPIO_PORT, USARTX_RX_PIN, LL_GPIO_PULL_UP);
    
    NVIC_SetPriority(USARTX_IRQn, 0);  
    NVIC_EnableIRQ(USARTX_IRQn);
    
    LL_USART_SetTransferDirection(USARTX, LL_USART_DIRECTION_TX_RX);

    LL_USART_ConfigCharacter(USARTX, LL_USART_DATAWIDTH_8B, LL_USART_PARITY_NONE, LL_USART_STOPBITS_1);
    LL_USART_SetOverSampling(USARTX, LL_USART_OVERSAMPLING_8);
    LL_USART_SetBaudRate(USARTX, SystemCoreClock, LL_USART_OVERSAMPLING_8, 115200); 

    LL_USART_Enable(USARTX);
    
    LL_USART_EnableIT_RXNE(USARTX);
    LL_USART_EnableIT_IDLE(USARTX);
    
    Init_ring_buffer(&RX_ringbuffer, RX_buffer, 256);
}

void UART_Enable_TX_DMA(uint8_t *pBuffer)
{
    /* Enable the clock of DMA1 */
    LL_AHB1_GRP1_EnableClock(LL_AHB1_GRP1_PERIPH_DMA1);

    /* Configure the DMA functional parameters for transmission */
    LL_DMA_ConfigTransfer(DMA1, LL_DMA_CHANNEL_7, 
                          LL_DMA_DIRECTION_MEMORY_TO_PERIPH | 
                          LL_DMA_PRIORITY_HIGH              | 
                          LL_DMA_MODE_NORMAL                | 
                          LL_DMA_PERIPH_NOINCREMENT         | 
                          LL_DMA_MEMORY_INCREMENT           | 
                          LL_DMA_PDATAALIGN_BYTE            | 
                          LL_DMA_MDATAALIGN_BYTE);
    LL_DMA_ConfigAddresses(DMA1, LL_DMA_CHANNEL_7,
                           (uint32_t)pBuffer,
                           LL_USART_DMA_GetRegAddr(USARTX),
                           LL_DMA_GetDataTransferDirection(DMA1, LL_DMA_CHANNEL_7));
    LL_USART_EnableDMAReq_TX(USART2);
}

void UART_Send_Byte(uint8_t data)
{
    /* Wait for TC flag to be raised for last char */
    while (!LL_USART_IsActiveFlag_TC(USARTX));
    LL_USART_TransmitData8(USARTX, data);
}

void UART_Send_Buffer(uint8_t *pBuffer, uint16_t length)
{
    uint16_t i;
    for(i=0; i<length; i++)
    {
        UART_Send_Byte(pBuffer[i]);
    }
}

void UART_Send_Buffer_DMA(uint16_t length)
{
    LL_DMA_DisableChannel(DMA1, LL_DMA_CHANNEL_7);
    LL_DMA_SetDataLength(DMA1, LL_DMA_CHANNEL_7, length);
    LL_DMA_EnableChannel(DMA1, LL_DMA_CHANNEL_7);
}

void UART_Enable_printf(bool enable)
{
    Is_printf_enabled = enable;
}

void UART_PRINTF( char *format, ... )
{
  if(Is_printf_enabled)
  {
      va_list args;
      va_start(args, format);
      uint16_t length;
      
      /*convert into string at buff[0] of length iw*/
      length = vsprintf(&buff[0], format, args);
      
      UART_Send_Buffer((uint8_t *)&buff[0], length);
      va_end(args);
  }
}

bool UART_IsNewFrameReceived(void)
{
    if(RX_flag)
    {
        if(Is_ring_buffer_empty(&RX_ringbuffer))
        {
            RX_flag = false;
        }
        else
        {
            return true;
        }
    }
    return false;
}

uint8_t UART_Read_Buffer(uint8_t *pBuffer, uint8_t size)
{
    return  Read_ring_buffer(&RX_ringbuffer, pBuffer, size);
}

void USARTX_IRQHandler(void)
{
    if(LL_USART_IsActiveFlag_RXNE(USARTX))
    {
//        LL_USART_ClearFlag_RXNE(USARTX);
        Write_ring_buffer_byte(&RX_ringbuffer, LL_USART_ReceiveData8(USARTX));
    }
    if(LL_USART_IsActiveFlag_IDLE(USARTX))
    {
        LL_USART_ClearFlag_IDLE(USARTX);
        RX_flag = true;
    }
}
