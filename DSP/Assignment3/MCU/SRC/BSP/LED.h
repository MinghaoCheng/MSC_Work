#ifndef __LED__H__
#define __LED__H__

#include "stm32l1xx_ll_bus.h"
#include "stm32l1xx_ll_rcc.h"
#include "stm32l1xx_ll_gpio.h"

void LED_Init(void);
void LED_Toggle(void);


#endif
