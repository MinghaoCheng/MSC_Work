#ifndef __FIR__H__
#define __FIR__H__

#include <stdint.h>
// _declspec(dllexport)
_declspec(dllexport) void init(float *coefficients, uint16_t tabs);
_declspec(dllexport) float dofilter(float v);
_declspec(dllexport) void reset_buffer(void);
_declspec(dllexport) void reset_all(void);


#endif