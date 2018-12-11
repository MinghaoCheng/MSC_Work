#include "clk.h"

void clk_HSE(void)
{
    LL_FLASH_Enable64bitAccess();
    LL_FLASH_SetLatency(LL_FLASH_LATENCY_1);

    /* Set Voltage scale1 as MCU will run at 32MHz */
    LL_APB1_GRP1_EnableClock(LL_APB1_GRP1_PERIPH_PWR);
    LL_PWR_SetRegulVoltageScaling(LL_PWR_REGU_VOLTAGE_SCALE1);

    /* Poll VOSF bit of in PWR_CSR. Wait until it is reset to 0 */
    while (LL_PWR_IsActiveFlag_VOSF() != 0);
    LL_RCC_EnableIT_HSERDY(); 
    LL_RCC_HSE_EnableCSS(); 
    LL_RCC_HSE_EnableBypass(); 
    LL_RCC_HSE_Enable();
    
    /* Clear the flag HSE ready */
    LL_RCC_ClearFlag_HSERDY();
    
    /* Disable the PLL */
    /* Wait until PLLRDY is cleared */
    LL_RCC_PLL_Disable();
    while(LL_RCC_PLL_IsReady() != 0);
    
    /* Configure PLL */
    LL_RCC_PLL_ConfigDomain_SYS(LL_RCC_PLLSOURCE_HSE, LL_RCC_PLL_MUL_8, LL_RCC_PLL_DIV_2);

    /* Enable the PLL */ 
    /* Wait until PLLRDY is set */
    LL_RCC_PLL_Enable();
    while(LL_RCC_PLL_IsReady() != 1);
    
    /* Switch the system clock to PLL */
    LL_RCC_SetSysClkSource(LL_RCC_SYS_CLKSOURCE_PLL);
    

    /* Set APB1 & APB2 prescaler*/
    LL_RCC_SetAPB1Prescaler(LL_RCC_APB1_DIV_1);
    LL_RCC_SetAPB2Prescaler(LL_RCC_APB2_DIV_1);
    
    while (LL_RCC_GetSysClkSource() != LL_RCC_SYS_CLKSOURCE_STATUS_PLL);
    

    
    /* Set systick to 1ms in using frequency set to 32MHz                             */
    /* This frequency can be calculated through LL RCC macro                          */
    /* ex: __LL_RCC_CALC_PLLCLK_FREQ (HSI_VALUE, LL_RCC_PLL_MUL_6, LL_RCC_PLL_DIV_3); */
    LL_Init1msTick(32000000);

    /* Update CMSIS variable (which can be updated also through SystemCoreClockUpdate function) */
    LL_SetSystemCoreClock(32000000);
      /* Enable ACC64 access and set FLASH latency */ 
}

void clk_HSI(void)
{
    /* Enable ACC64 access and set FLASH latency */ 
    LL_FLASH_Enable64bitAccess();; 
    LL_FLASH_SetLatency(LL_FLASH_LATENCY_1);

    /* Set Voltage scale1 as MCU will run at 32MHz */
    LL_APB1_GRP1_EnableClock(LL_APB1_GRP1_PERIPH_PWR);
    LL_PWR_SetRegulVoltageScaling(LL_PWR_REGU_VOLTAGE_SCALE1);

    /* Poll VOSF bit of in PWR_CSR. Wait until it is reset to 0 */
    while (LL_PWR_IsActiveFlag_VOSF() != 0);
    
    /* Enable HSI if not already activated*/
    if (LL_RCC_HSI_IsReady() == 0)
    {
        /* HSI configuration and activation */
        LL_RCC_HSI_Enable();
        while(LL_RCC_HSI_IsReady() != 1);
    }

    /* Main PLL configuration and activation */
    LL_RCC_PLL_ConfigDomain_SYS(LL_RCC_PLLSOURCE_HSI, LL_RCC_PLL_MUL_6, LL_RCC_PLL_DIV_3);

    LL_RCC_PLL_Enable();
    while(LL_RCC_PLL_IsReady() != 1);

    /* Sysclk activation on the main PLL */
    LL_RCC_SetAHBPrescaler(LL_RCC_SYSCLK_DIV_1);
    LL_RCC_SetSysClkSource(LL_RCC_SYS_CLKSOURCE_PLL);
    while(LL_RCC_GetSysClkSource() != LL_RCC_SYS_CLKSOURCE_STATUS_PLL);

    /* Set APB1 & APB2 prescaler*/
    LL_RCC_SetAPB1Prescaler(LL_RCC_APB1_DIV_1);
    LL_RCC_SetAPB2Prescaler(LL_RCC_APB2_DIV_1);

    /* Set systick to 1ms in using frequency set to 32MHz                             */
    /* This frequency can be calculated through LL RCC macro                          */
    /* ex: __LL_RCC_CALC_PLLCLK_FREQ (HSI_VALUE, LL_RCC_PLL_MUL_6, LL_RCC_PLL_DIV_3); */
    LL_Init1msTick(32000000);

    /* Update CMSIS variable (which can be updated also through SystemCoreClockUpdate function) */
    LL_SetSystemCoreClock(32000000);
}
