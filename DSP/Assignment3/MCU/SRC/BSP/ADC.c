#include "adc.h"

void ADC_Init(void)
{
    /*## Configuration of GPIO used by ADC channels ############################*/
    /* Note: On this STM32 device, ADC1 channel 4 is mapped on GPIO pin PA.04 */ 
    /* Enable GPIO Clock */
    LL_AHB1_GRP1_EnableClock(LL_AHB1_GRP1_PERIPH_GPIOA);

    /* Configure GPIO in analog mode to be used as ADC input */
    LL_GPIO_SetPinMode(GPIOA, LL_GPIO_PIN_4, LL_GPIO_MODE_ANALOG);

    /*## Configuration of NVIC #################################################*/
    /* Configure NVIC to enable ADC1 interruptions */
    NVIC_SetPriority(ADC1_IRQn, 0);
    NVIC_EnableIRQ(ADC1_IRQn);

    /*## Configuration of ADC ##################################################*/

    /*## Configuration of ADC hierarchical scope: common to several ADC ########*/

    /* Enable ADC clock (core clock) */
    LL_APB2_GRP1_EnableClock(LL_APB2_GRP1_PERIPH_ADC1);
    if(__LL_ADC_IS_ENABLED_ALL_COMMON_INSTANCE() == 0)
    {
        /* Note: Call of the functions below are commented because they are       */
        /*       useless in this example:                                         */
        /*       setting corresponding to default configuration from reset state. */
    
        /* Set ADC clock (conversion clock) common to several ADC instances */
        LL_ADC_SetCommonClock(__LL_ADC_COMMON_INSTANCE(ADC1), LL_ADC_CLOCK_ASYNC_DIV2);
    
        /* Set ADC measurement path to internal channels */
        // LL_ADC_SetCommonPathInternalCh(__LL_ADC_COMMON_INSTANCE(ADC1), LL_ADC_PATH_INTERNAL_NONE);
    }
    /*## Configuration of ADC hierarchical scope: ADC instance #################*/
    if (LL_ADC_IsEnabled(ADC1) == 0)
    {
        /* Note: Call of the functions below are commented because they are       */
        /*       useless in this example:                                         */
        /*       setting corresponding to default configuration from reset state. */

        /* Set ADC data resolution */
        LL_ADC_SetResolution(ADC1, LL_ADC_RESOLUTION_12B);

        /* Set ADC conversion data alignment */
        // LL_ADC_SetResolution(ADC1, LL_ADC_DATA_ALIGN_RIGHT);

        /* Set ADC low power mode */
        // LL_ADC_SetLowPowerMode(ADC1, LL_ADC_LP_MODE_NONE);

        /* Set Set ADC sequencers scan mode, for all ADC groups                   */
        /* (group regular, group injected).                                       */
        // LL_ADC_SetSequencersScanMode(ADC1, LL_ADC_SEQ_SCAN_DISABLE);

        /* Set ADC channels bank */
        // LL_ADC_SetChannelsBank(ADC1, LL_ADC_CHANNELS_BANK_A);
    
    }
    /*## Configuration of ADC hierarchical scope: ADC group regular ############*/
    if (LL_ADC_IsEnabled(ADC1) == 0)
    {
        /* Set ADC group regular trigger source */
        LL_ADC_REG_SetTriggerSource(ADC1, LL_ADC_REG_TRIG_EXT_TIM2_TRGO);

        /* Set ADC group regular trigger polarity */
        // LL_ADC_REG_SetTriggerEdge(ADC1, LL_ADC_REG_TRIG_EXT_RISING);

        /* Set ADC group regular continuous mode */
        LL_ADC_REG_SetContinuousMode(ADC1, LL_ADC_REG_CONV_SINGLE);

        /* Set ADC group regular conversion data transfer */
        // LL_ADC_REG_SetDMATransfer(ADC1, LL_ADC_REG_DMA_TRANSFER_NONE);

        /* Specify which ADC flag between EOC (end of unitary conversion)         */
        /* or EOS (end of sequence conversions) is used to indicate               */
        /* the end of conversion.                                                 */
        // LL_ADC_REG_SetFlagEndOfConversion(ADC1, LL_ADC_REG_FLAG_EOC_SEQUENCE_CONV);

        /* Set ADC group regular sequencer */
        /* Note: On this STM32 serie, ADC group regular sequencer is              */
        /*       fully configurable: sequencer length and each rank               */
        /*       affectation to a channel are configurable.                       */
        /*       Refer to description of function                                 */
        /*       "LL_ADC_REG_SetSequencerLength()".                               */

        /* Set ADC group regular sequencer length and scan direction */
        LL_ADC_REG_SetSequencerLength(ADC1, LL_ADC_REG_SEQ_SCAN_DISABLE);

        /* Set ADC group regular sequencer discontinuous mode */
        // LL_ADC_REG_SetSequencerDiscont(ADC1, LL_ADC_REG_SEQ_DISCONT_DISABLE);

        /* Set ADC group regular sequence: channel on the selected sequence rank. */
        LL_ADC_REG_SetSequencerRanks(ADC1, LL_ADC_REG_RANK_1, LL_ADC_CHANNEL_4);
    }
  
  
    /*## Configuration of ADC hierarchical scope: ADC group injected ###########*/

    /* Note: Hardware constraint (refer to description of the functions         */
    /*       below):                                                            */
    /*       On this STM32 serie, setting of these features are not             */
    /*       conditioned to ADC state.                                          */
    /*       However, ADC state is checked anyway with standard requirements    */
    /*       (refer to description of this function).                           */
    if (LL_ADC_IsEnabled(ADC1) == 0)
    {
        /* Note: Call of the functions below are commented because they are       */
        /*       useless in this example:                                         */
        /*       setting corresponding to default configuration from reset state. */

        /* Set ADC group injected trigger source */
        // LL_ADC_INJ_SetTriggerSource(ADC1, LL_ADC_INJ_TRIG_SOFTWARE);

        /* Set ADC group injected trigger polarity */
        // LL_ADC_INJ_SetTriggerEdge(ADC1, LL_ADC_INJ_TRIG_EXT_RISING);

        /* Set ADC group injected conversion trigger  */
        // LL_ADC_INJ_SetTrigAuto(ADC1, LL_ADC_INJ_TRIG_INDEPENDENT);

        /* Set ADC group injected sequencer */
        /* Note: On this STM32 serie, ADC group injected sequencer is             */
        /*       fully configurable: sequencer length and each rank               */
        /*       affectation to a channel are configurable.                       */
        /*       Refer to description of function                                 */
        /*       "LL_ADC_INJ_SetSequencerLength()".                               */

        /* Set ADC group injected sequencer length and scan direction */
        // LL_ADC_INJ_SetSequencerLength(ADC1, LL_ADC_INJ_SEQ_SCAN_DISABLE);

        /* Set ADC group injected sequencer discontinuous mode */
        // LL_ADC_INJ_SetSequencerDiscont(ADC1, LL_ADC_INJ_SEQ_DISCONT_DISABLE);

        /* Set ADC group injected sequence: channel on the selected sequence rank. */
        // LL_ADC_INJ_SetSequencerRanks(ADC1, LL_ADC_INJ_RANK_1, LL_ADC_CHANNEL_4);
    }
  
  
  /*## Configuration of ADC hierarchical scope: channels #####################*/
    if (LL_ADC_IsEnabled(ADC1) == 0)
    {
        LL_ADC_SetChannelSamplingTime(ADC1, LL_ADC_CHANNEL_4, LL_ADC_SAMPLINGTIME_48CYCLES);
    }
    /*## Configuration of ADC interruptions ####################################*/
    LL_ADC_EnableIT_EOCS(ADC1);
    /* Enable interruption ADC group regular overrun */
    LL_ADC_EnableIT_OVR(ADC1);
}

void ADC_Start(void)
{
    /* Enable ADC clock (conversion clock) */
    /* On this STM32 serie, ADC is using a dedicated asynchronous clock         */
    /* derived from HSI oscillator.                                             */
    /* Activate HSI if not already activated */
    if (LL_RCC_HSI_IsReady() == 0)
    {
        /* HSI enable */
        LL_RCC_HSI_Enable();
        /* Poll for HSI effectively activated */
        while(LL_RCC_HSI_IsReady() != 1);
    }
    if (LL_ADC_IsEnabled(ADC1) == 0)
    {
        /* Enable ADC */
        LL_ADC_Enable(ADC1);
    }
    LL_ADC_REG_StartConversionExtTrig(ADC1, LL_ADC_REG_TRIG_EXT_RISING);
}
