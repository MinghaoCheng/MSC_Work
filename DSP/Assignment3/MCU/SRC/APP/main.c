#include "main.h"

int main(void)
{
    /* hardware init */
    clk_HSE();
    Timer_Init();
    ADC_Init();
    UART_Init();
    LED_Init();
    
    /* Midware init */
    Protocol_Init();
    
    /* Start ADC */
    ADC_Start();
    
    while (1);
}

void ADC_complete_callback(void)
{
    uint16_t mVolt;
    mVolt = __LL_ADC_CALC_DATA_TO_VOLTAGE(3300, LL_ADC_REG_ReadConversionData12(ADC1), LL_ADC_RESOLUTION_12B);
    Protocol_send_packet(mVolt);
    LED_Toggle();
}

void ADC1_IRQHandler(void)
{
    if(LL_ADC_IsActiveFlag_EOCS(ADC1) != 0)
    {
        /* Clear flag ADC group regular end of unitary conversion */
        LL_ADC_ClearFlag_EOCS(ADC1);

        /* Call interruption treatment function */
        ADC_complete_callback();
    }
    if(LL_ADC_IsActiveFlag_OVR(ADC1) != 0)
    {
        /* Clear flag ADC group regular overrun */
        LL_ADC_ClearFlag_OVR(ADC1);
    }
}
