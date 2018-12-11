/* Includes ------------------------------------------------------------------*/
//#include "timeServer.h"
#include "delay.h"

void DelayMs( uint32_t ms )
{
    uint32_t i,j;
    for(i=0; i<ms; i++)
    {
        for(j=0; j<3200; j++)
        {
            asm("nop");
        }
    }
}

void Delay( float s )
{
    DelayMs( (uint32_t) (s * 1000.0f) );
}

/************************ (C) COPYRIGHT STMicroelectronics *****END OF FILE****/

