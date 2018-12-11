#ifndef _BOARD_CONF_
#define _BOARD_CONF_

/* --------------------------- USART HW definition -------------------------------*/
/* Definition for USARTx Pins */
#define USARTX_TX_PIN                             LL_GPIO_PIN_2
#define USARTX_TX_GPIO_PORT                       GPIOA  
#define USARTX_TX_AF                              LL_GPIO_AF_7
#define USARTX_RX_PIN                             LL_GPIO_PIN_3
#define USARTX_RX_GPIO_PORT                       GPIOA 
#define USARTX_RX_AF                              LL_GPIO_AF_7
/* Definition for USARTx's NVIC */
#define USARTX                                    USART2
#define USARTX_IRQn                               USART2_IRQn
#define USARTX_IRQHandler                         USART2_IRQHandler

#endif
