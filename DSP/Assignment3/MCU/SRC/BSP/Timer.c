#include "Timer.h"

void Timer_Init(void)
{
    /* Parameters of timer (used as ADC conversion trigger) */
    /* Timer frequency (unit: Hz). With a timer 16 bits and time base           */
    /* freq min 1Hz, range is min=1Hz, max=32kHz.                               */
    #define TIMER_FREQUENCY                ((uint32_t) 300)
    /* Timer minimum frequency (unit: Hz), used to calculate frequency range.   */
    /* With a timer 16 bits, maximum frequency will be 32000 times this value.  */
    #define TIMER_FREQUENCY_RANGE_MIN      ((uint32_t)    1)
    /* Timer prescaler maximum value (0xFFFF for a timer 16 bits)               */
    #define TIMER_PRESCALER_MAX_VALUE      ((uint32_t)0xFFFF-1)

    uint32_t timer_clock_frequency = 0;             /* Timer clock frequency */
    uint32_t timer_prescaler = 0;                   /* Time base prescaler to have timebase aligned on minimum frequency possible */
    uint32_t timer_reload = 0;                      /* Timer reload value in function of timer prescaler to achieve time base period */

    /* If APB1 prescaler is different of 1, timers have a factor x2 on their    */
    /* clock source.                                                            */
    if (LL_RCC_GetAPB1Prescaler() == LL_RCC_APB1_DIV_1)
    {
        timer_clock_frequency = __LL_RCC_CALC_PCLK1_FREQ(SystemCoreClock, LL_RCC_GetAPB1Prescaler());
    }
    else
    {
        timer_clock_frequency = (__LL_RCC_CALC_PCLK1_FREQ(SystemCoreClock, LL_RCC_GetAPB1Prescaler()) * 2);
    }

    /* Timer prescaler calculation */
    /* (computation for timer 16 bits, additional + 1 to round the prescaler up) */
    timer_prescaler = ((timer_clock_frequency / (TIMER_PRESCALER_MAX_VALUE * TIMER_FREQUENCY_RANGE_MIN)) +1);
    /* Timer reload calculation */
    timer_reload = (timer_clock_frequency / (timer_prescaler * TIMER_FREQUENCY));

    /* Enable the timer peripheral clock */
    LL_APB1_GRP1_EnableClock(LL_APB1_GRP1_PERIPH_TIM2);

    /* Set timer pre-scaler value */
    LL_TIM_SetPrescaler(TIM2, (timer_prescaler - 1));

    /* Set timer auto-reload value */
    LL_TIM_SetAutoReload(TIM2, (timer_reload - 1));

    /* Counter mode: select up-counting mode */
    LL_TIM_SetCounterMode(TIM2, LL_TIM_COUNTERMODE_UP);

    /* Note: In this example, timer interrupts are not activated.               */
    /*       If needed, timer interruption at each time base period is          */
    /*       possible.                                                          */
    /*       Refer to timer examples.                                           */

    /* Set timer the trigger output (TRGO) */
    LL_TIM_SetTriggerOutput(TIM2, LL_TIM_TRGO_UPDATE);

    /* Enable the update interrupt */
    LL_TIM_EnableIT_UPDATE(TIM2);

    /* Configure the NVIC to handle TIM2 update interrupt */
    //NVIC_SetPriority(TIM2_IRQn, 0);
    //NVIC_EnableIRQ(TIM2_IRQn);

    /* Enable counter */
    LL_TIM_EnableCounter(TIM2);
    /* Force update generation */
    //LL_TIM_GenerateEvent_UPDATE(TIM2);
}
