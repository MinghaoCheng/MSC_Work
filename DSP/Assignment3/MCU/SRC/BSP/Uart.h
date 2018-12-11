#ifndef _UART_BOARD_H_
#define _UART_BOARD_H_

#include <stdint.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stdio.h>

#include "board_conf.h"

#include "stm32l1xx_ll_bus.h"
#include "stm32l1xx_ll_rcc.h"
#include "stm32l1xx_ll_gpio.h"
#include "stm32l1xx_ll_usart.h"
#include "stm32l1xx_ll_dma.h"

void UART_Init(void);
void UART_Enable_TX_DMA(uint8_t *pBuffer);
void UART_Send_Byte(uint8_t data);
void UART_Send_Buffer(uint8_t *pBuffer, uint16_t length);
void UART_Send_Buffer_DMA(uint16_t length);
void UART_Enable_printf(bool enable);
void UART_PRINTF(char *format, ...);
bool UART_IsNewFrameReceived(void);
uint8_t UART_Read_Buffer(uint8_t *pBuffer, uint8_t size);

#endif
