#include "FIR.h"

static float buffer[65535];
static float b[65535];
static float *pbuffer, *buffer_end;
static uint16_t ntabs;

void init(float *coefficients, uint16_t tabs)
{
	uint16_t i;
	ntabs = tabs;
	pbuffer = buffer;
	buffer_end = &buffer[ntabs - 1];
	
	for (i = 0; i < ntabs; i++)
	{
		b[i] = coefficients[i];
	}
}

float dofilter(float v)
{
	float out = 0;
	float *buffer_value = pbuffer;
	float *b_value = b;

	*pbuffer = v;

	while (buffer_value <= buffer_end)
	{
		out += *buffer_value++ * *b_value++;
	}

	buffer_value = buffer;

	while (buffer_value < pbuffer)
	{
		out += *buffer_value++ * *b_value++;
	}

	pbuffer--;
	if (pbuffer < buffer)
	{
		pbuffer = buffer_end;
	}
	return out;
}

void reset_buffer(void)
{
	uint16_t i;

	for (i = 0; i < ntabs; i++)
	{
		buffer[i] = 0;
	}
}

void reset_all(void)
{
	uint16_t i;
	for (i = 0; i < ntabs; i++)
	{
		buffer[i] = 0;
		b[i] = 0;
	}
	ntabs = 0;
}