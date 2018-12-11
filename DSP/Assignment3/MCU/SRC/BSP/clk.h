#ifndef _CLK_H_
#define _CLK_H_

#include "stm32l1xx_ll_bus.h"
#include "stm32l1xx_ll_rcc.h"
#include "stm32l1xx_ll_pwr.h"
#include "stm32l1xx_ll_system.h"
#include "stm32l1xx_ll_utils.h"
#include "stm32l1xx_ll_gpio.h"
#include "stm32l1xx_ll_tim.h"

void clk_HSE(void);
void clk_HSI(void);

#endif
