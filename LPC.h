#ifndef _LPC_
#define _LPC_

#include <assert.h>
namespace LPC
{
	typedef unsigned int uint32;
	/**
	* input:   model order
	*          input length
	*          input buffer
	*          can be input buffer
	*          parcorf/parcorb should be 
	*          result
	*/
	void arburg(uint32 modelorder, uint32 const inplen, float *const inp, float *parcorf, float *parcorb,float *result)
	{
		if (modelorder>1)
		{
			//modelorder--;
			float E = 0;

			for (uint32 i=0;i<modelorder+1;i++)
			{
				result[i]=0;
			}

			for (uint32 i=0;i<inplen;i++)
			{
				parcorf[i]      =   inp[i];
				parcorb[i]      =   inp[i];
				E               +=  inp[i]*inp[i];
			}

			E /= inplen;                            //normalization
			result[0]           =   1;

			for (uint32 i=0;i<modelorder;i++)
			{
				float *efp  =   &parcorf[i+1];            //initially parcorf from index 1 (2nd element) to end
				float *ebp  =   &parcorb[0];              //initially parcorb from index 0 to penultimate (2nd last)
				float *efpp =   efp;
				float *ebpp =   ebp;
				float num   =   0;
				float den   =   0;

				for (uint32 j=0;j<inplen-i-1;j++)
				{
					float ef = *(efpp++);
					float eb = *(ebpp++);
					num     += ef*eb;
					den     += ef*ef+eb*eb;
				}

				float k     =   static_cast<float>(-2.0)*num/den;
				for (uint32 j=0;j<inplen-i-1;j++)
				{
					float ef = *efp+k**ebp;
					float eb = *ebp+k**efp;
					*efp     = ef;
					*ebp     = eb;                      
					efp++;
					ebp++;
				}

				//update ar coefficients, we need an in-place implementation
				for (uint32 j=0;j<((i+1)>>1)+1;j++)
				{
					//forward and backward coefficient need to be calculated at the same time
					float forward   = result[j]     +k*result[i-j+1];
					float backward  = result[i-j+1] +k*result[j];
					result[j]       = forward;
					result[i-j+1]   = backward;
				}
				E *= (1-k*k);
			}
		}
		else if (modelorder==1)
		{

		}
		else
		{
			result[0]=1.0;
		}
	}

	bool BuildModel(size_t inplength, short *buffer, size_t modelorder, float *model)
	{
		bool retVal = false;
		if (inplength<2048)
		{
			static float inp[2048];
			static float parcorf[2048];
			static float parcorb[2048];

			for (size_t i=0;i<inplength;i++)
				inp[i]=buffer[i];

			arburg(static_cast<unsigned int>(modelorder), static_cast<unsigned int>(inplength), inp, parcorf,parcorb,model);

			retVal = true;
		}
		return retVal;
	}

	bool Decorrelate(size_t length, short *inpbuffer, size_t nocoef, const int scaling, short *coefficients, short *outbuffer)
	{
		assert(inpbuffer);
		assert(outbuffer);
		bool retVal = false;

		if (length>0)
		{
			outbuffer[0] = inpbuffer[0];					//that is the first one, skip all the multiplications
			for (size_t i=1;i<nocoef;i++)
			{
				int accu = 0;
				for (size_t j=0;j<i;j++)
				{
					accu += inpbuffer[j]*coefficients[i-1-j];
				}
				outbuffer[i] = inpbuffer[i]-(accu>>scaling);
			}

			for (size_t i=nocoef;i<length;i++)
			{
				int accu=0;
				for (size_t j=0;j<nocoef;j++)
				{
					accu+=inpbuffer[i-nocoef+j]*coefficients[nocoef-1-j];
				}
				accu =  inpbuffer[i]-(accu>>scaling);
				outbuffer[i] = static_cast<short>(accu);
			}
			retVal = true;
		}
		return retVal;
	}

	bool Correlate(size_t length, short *inpbuffer, size_t nocoef, const int scaling, short *coefficients, short *outbuffer)
	{
		assert(inpbuffer);
		assert(outbuffer);
		bool retVal = false;

		if (length>1)
		{
			outbuffer[0] = inpbuffer[0];					//that is the first one, skip all the multiplications

			for (size_t i=1;i<nocoef;i++)
			{
				int accu = 0;
				for (size_t j=0;j<i;j++)
				{
					accu += outbuffer[j]*coefficients[i-1-j];
				}
				outbuffer[i] = inpbuffer[i]+(accu>>scaling);
			}

			for (size_t i=nocoef;i<length;i++)
			{
				int accu=0;
				for (size_t j=0;j<nocoef;j++)
				{
					accu+=outbuffer[i-nocoef+j]*coefficients[nocoef-1-j];
				}
				accu =  inpbuffer[i]+(accu>>scaling);
				outbuffer[i] = static_cast<short>(accu);
			}
			retVal = true;
		}
		return retVal;
	}
}
#endif