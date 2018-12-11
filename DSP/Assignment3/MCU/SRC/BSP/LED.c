#include "LED.h"

void LED_Init(void)
{
    /* Configure IO in output push-pull mode to drive external LED2 */
    LL_AHB1_GRP1_EnableClock(LL_AHB1_GRP1_PERIPH_GPIOA);
    LL_GPIO_SetPinMode(GPIOA, LL_GPIO_PIN_5, LL_GPIO_MODE_OUTPUT);
}

void LED_Toggle(void)
{
    // Toggle LED
    LL_GPIO_TogglePin(GPIOA, LL_GPIO_PIN_5);
}