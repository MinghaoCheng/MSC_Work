#ifndef _ADC_H_
#define _ADC_H_

#include <stdint.h>

#include "stm32l1xx_ll_bus.h"
#include "stm32l1xx_ll_rcc.h"
#include "stm32l1xx_ll_gpio.h"
#include "stm32l1xx_ll_adc.h"

void ADC_Init(void);
void ADC_Start(void);

#endif
