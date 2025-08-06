#ifndef _ST_ADAPTIVE_ARITHMETIC_CODEC_
#define _ST_ADAPTIVE_ARITHMETIC_CODEC_
namespace STAdaptiveArithmeticCodec {

	typedef enum tageeErrorCode{eNoERROR, eINVALIDTOKEN, eINVALIDDATABUFFER, eINSUFFICIENTMEMORY, eDECODINGINCOMPLETE}eErrorCode;

	template <typename T, typename TDataType, size_t dictsize> class CBaseEntropyTracker
	{
		private:
			TDataType		 *mp_Data;
			size_t			  m_MaxInputLength;
			size_t			  m_Iterator;
		protected:
			static const size_t EOFStats=1;
			static const size_t XDirectorySize=dictsize+EOFStats;
		public:
			static const size_t EOFIndicator=dictsize;
		
		public:
			CBaseEntropyTracker(){};
			virtual ~CBaseEntropyTracker(){};
			TDataType
					GetSymbol()const{return mp_Data[m_Iterator];}
			size_t	GetIterator()const{return m_Iterator;}
			
			void	Init(size_t len, TDataType *inp){assert(mp_Data);assert(m_MaxInputLength);m_Iterator=0;m_MaxInputLength=len;mp_Data=inp;};

			virtual eErrorCode CreateStatistics(size_t len, TDataType *buffer)=0;
			virtual eErrorCode GetSymbolStatistics(size_t symbol, T *symbolstats, T *pRange)const=0;
			virtual size_t	   UpdateStatistics(){return ++m_Iterator;}
			virtual size_t	   UpdateStatistics(size_t Symbol){mp_Data[m_Iterator++]=Symbol;return m_Iterator;}
			virtual size_t	   GetSymbol(T statistics)=0;
			
	};

	template <typename T, typename TDataType, size_t dictsize> class CEntropyTracker:public CBaseEntropyTracker<T,TDataType,dictsize>{
	private:		
		T m_Dictonary[XDirectorySize];

		T GetSymbolRange(TDataType symbol)const{
			T lrange=m_Dictonary[symbol];
			for (size_t i=symbol+1;i<sizeof(m_Dictonary)/sizeof(m_Dictonary[0])&&m_Dictonary[i]!=0;i++){
				const T range = m_Dictonary[i]-lrange;
				if (range)
				{
					return range;
				}
			}
			return 0-lrange;
		}
	protected:
	public:
		CEntropyTracker(){memset(m_Dictonary,0,sizeof(m_Dictonary));}
		virtual ~CEntropyTracker(){}
		eErrorCode CreateStatistics(size_t len, TDataType *buffer)
		{
			assert(buffer);
			eErrorCode retVal;
			if (len>0&&buffer)
			{
				retVal = eNoERROR;
				memset(m_Dictonary,0,sizeof(m_Dictonary));

				for (size_t i=0;i<len;i++)
				{
					if (buffer[i]>=dictsize)
					{
						retVal = eINVALIDTOKEN;
						break;
					}
				}

				if (retVal==eNoERROR)
				{
					for (size_t i=0;i<len;i++)
					{
						m_Dictonary[buffer[i]]++;
					}
					m_Dictonary[EOFIndicator]=EOFStats;

					T History=0;
					len+=EOFStats;
					for (size_t i=0;i<XDirectorySize;i++)
					{
						const T val = m_Dictonary[i];
						m_Dictonary[i] = History;
						const T dummy = static_cast<T>(0)-static_cast<T>(len);
						History = ((dummy/len)+1)*val+History;
						//std::cout << (unsigned int)m_Dictonary[i] << std::endl;
					}
				}
			}
			else if (!buffer)
			{
				retVal = eINVALIDDATABUFFER;
			}
			else
			{
				retVal = eINSUFFICIENTMEMORY;
			}
			return retVal;
		}

		eErrorCode GetSymbolStatistics(size_t symbol, T *symbolstats, T *pRange)const{assert(symbolstats);assert(pRange);*symbolstats=m_Dictonary[symbol];*pRange=GetSymbolRange(symbol);return eNoERROR;}

		size_t GetSymbol(T statistics)
		{
			assert(dictsize>1);
			for (size_t i=0;i<sizeof(m_Dictonary)/sizeof(m_Dictonary[0])-1;++i)
			{
				if (m_Dictonary[i]!=m_Dictonary[i+1])
				{
					if (statistics>=m_Dictonary[i]&&(statistics<m_Dictonary[i+1]||m_Dictonary[i+1]==0))
					{
						return i;
					}
				}
			}
			return sizeof(m_Dictonary)/sizeof(m_Dictonary[0])-1;
		}
	};

	template <typename T, typename TAccType, typename TDataType, size_t dictsize> class CCodec{
	private:
		CBaseEntropyTracker<T,TDataType,dictsize> &m_Tracker;
		CCodec(){};
	protected:
	public:
		explicit CCodec(CBaseEntropyTracker<T,TDataType, dictsize> &Tracker):m_Tracker(Tracker){}
		~CCodec(){}
		eErrorCode CreateStatistics(size_t len, TDataType *buffer)
		{
			return m_Tracker.CreateStatistics(len,buffer);
		}

		eErrorCode Compress(size_t len, TDataType *buffer, size_t maxoutlen, T *result, size_t *pOutplen, size_t *pMask)
		{
			eErrorCode retVal;
			if (len&&maxoutlen)
			{
				static const T		THighestBit			= static_cast<T>(1)<<(sizeof(T)*8-1);
				static const size_t	TMaxShifts			= sizeof(T)*8;
				static const size_t	TAccTypeMaxShifts	= sizeof(TAccType)*8;
				static const size_t	TNormalizationShifts= TAccTypeMaxShifts-TMaxShifts;

				m_Tracker.Init(len,buffer);

				memset(result,0,maxoutlen*sizeof(T));
				size_t outp         = 0;
				result[outp]        = 0;
				
				T lower, range;
				m_Tracker.GetSymbolStatistics(m_Tracker.GetSymbol(),&lower,&range);
				T upper             = lower+range;
				size_t shift        = 0;

				//flush out first code word
				while(static_cast<T>(range)<=THighestBit&&range!=0)
				{
					result[outp]|=(lower&THighestBit)>>(shift++);
					shift &= (TMaxShifts-1);
					outp += (shift==0);
					lower <<= 1;
					range <<= 1;
				}
				//first word flushed, now we have a new range and some left bits
				TAccType acc	 = static_cast<TAccType>(lower)<<TNormalizationShifts;
				TAccType longrange;
				longrange        = (static_cast<TAccType>(range)<<(TNormalizationShifts-TMaxShifts));
				//range is scaled to maximum, acc is scaled to maximum
				//-1 in the loop prevent us from checking it in the while loops at the bottom of the loop
				for (size_t i=m_Tracker.UpdateStatistics();i<len&&outp<maxoutlen-1;i=m_Tracker.UpdateStatistics())
				{
					TAccType lacc;
					TAccType uacc;
					if (longrange==0)
					{
						m_Tracker.GetSymbolStatistics(m_Tracker.GetSymbol(),&lower,&range);
						upper = lower+range;
						lacc = acc+(static_cast<TAccType>(lower)<<TNormalizationShifts);
						uacc = acc+(static_cast<TAccType>(upper)<<TNormalizationShifts);
					}
					else
					{
						TAccType Carry;
						m_Tracker.GetSymbolStatistics(m_Tracker.GetSymbol(),&lower,&range);
						upper = lower+range;
						Carry = lower<upper?upper:(static_cast<TAccType>(1)<<TMaxShifts)+upper;
						lacc = acc+static_cast<TAccType>(longrange)*lower;
						uacc = acc+static_cast<TAccType>(longrange)*Carry;
					}

					T Carry = lacc<acc;
					if (Carry)
					{
						T CurrPos = outp;
						if (shift==0)
						{
							CurrPos--;
							Carry=1;
						}
						else
						{
							Carry = THighestBit>>(shift-1);
						}
						do{
							T res = result[CurrPos];
							T output = res+Carry;
							result[CurrPos--] = output;
							Carry = output<res?1:0;
						}while(Carry);
					}
					acc = lacc;
					//range can be large as it is defined as a 2^(2*x) format
					longrange                = static_cast<TAccType>(uacc)-static_cast<TAccType>(lacc);
					range                        = longrange>>TNormalizationShifts;
					//range is at least 1. Flushing more than 8*size(T) bits does not make sense
					lower = lacc>>TNormalizationShifts;
					if (range)
					{
						while(static_cast<T>(range)<=THighestBit&&range!=0)
						{
							result[outp]|=(lower&THighestBit)>>(shift++);
							shift &= (TMaxShifts-1);
							outp += (shift==0);
							lower <<= 1;
							acc<<=1;
							longrange <<= 1;
							range <<= 1;
						}
					}
					else
					{
						for (size_t i=0;i<TMaxShifts;i++)
						{
							result[outp]|=(lower&THighestBit)>>(shift++);
							shift &= (TMaxShifts-1);
							outp += (shift==0);
							lower <<= 1;
							acc<<=1;
							longrange <<= 1;
						}
					}
					longrange>>=TMaxShifts;
				}
				if (outp<maxoutlen)
				{
					//acc contains the lower bound
					//the truncated upper bound is:
					TAccType alower;
					TAccType aupper;

					if (longrange==0)
					{
						m_Tracker.GetSymbolStatistics(CEntropyTracker<T,TDataType,dictsize>::EOFIndicator,&lower,&range);
						upper = lower+range;
						alower = acc+(static_cast<TAccType>(lower)<<TNormalizationShifts);
						aupper = acc+(static_cast<TAccType>(upper)<<TNormalizationShifts);
					}
					else
					{
						TAccType Carry;
						m_Tracker.GetSymbolStatistics(CEntropyTracker<T,TDataType,dictsize>::EOFIndicator,&lower,&range);
						upper = lower+range;
						Carry = lower<upper?upper:(static_cast<TAccType>(1)<<TMaxShifts)+upper;
						alower = acc+static_cast<TAccType>(longrange)*lower;
						aupper = acc+static_cast<TAccType>(longrange)*Carry;
					}
					aupper--;
					//is there an overflow
					T Carry = alower<acc;
					if (Carry)
					{
						T CurrPos = outp;
						if (shift==0)
						{
							CurrPos--;
							Carry=1;
						}
						else
						{
							Carry = THighestBit>>(shift-1);
						}
						do{
							T res = result[CurrPos];
							T output = res+Carry;
							result[CurrPos--] = output;
							Carry = output<res?1:0;
						}while(Carry);
					}


					//alower and aupper contain valid data words
					//1) aupper is higher than alower -> flush out until bits are unequal
					//2) aupper is smaller than alower -> set carry, increment number, set acc/lacc to zero and flush until bits are unequal
					
					//everything is allowed between lower and upper
					//if carry is set I add something and I am done...
					Carry = aupper<alower;
					if (Carry)
					{
						T CurrPos = outp;
						if (shift==0)
						{
							CurrPos--;
							Carry=1;
						}
						else
						{
							Carry = THighestBit>>(shift-1);
						}
						do{
							T res = result[CurrPos];
							T output = res+Carry;
							result[CurrPos--] = output;
							Carry = output<res?1:0;
						}while(Carry);

						alower = 0;
						TAccType flush = alower^aupper;
						bool mask = false;
						while(!mask&&outp<maxoutlen)
						{
							result[outp]|=((aupper>>TNormalizationShifts)&THighestBit)>>(shift++);
							mask                = ((aupper^alower)>>(TAccTypeMaxShifts-1))==1;//true if different
							shift &= (TMaxShifts-1);
							outp += (shift==0);
							alower <<= 1;
							aupper <<= 1;
						}
					}
					else
					{
						TAccType flush = alower^aupper;
						bool mask = false;
						while(!mask&&outp<maxoutlen)
						{
							result[outp]|=((aupper>>TNormalizationShifts)&THighestBit)>>(shift++);
							mask                = ((aupper^alower)>>(TAccTypeMaxShifts-1))==1;//true if different
							shift &= (TMaxShifts-1);
							outp += (shift==0);
							alower <<= 1;
							aupper <<= 1;
						}
					}


					if (outp>maxoutlen||((outp==maxoutlen)&&(shift)))
					{
						if (pOutplen)
						{
							*pOutplen = 0;
						}
						if (pMask)
						{
							*pMask	  = 0;
						}
						retVal = eINSUFFICIENTMEMORY;
					}
					else
					{
						if (pMask)
						{
							size_t reqshifts = 0;
							for (;shift!=0;shift<<=1){
								reqshifts++;
							}

							//divide by 8 (bytesize)
							*pMask	  = (reqshifts+7)>>3;
						} 

						if (pOutplen)
						{
							*pOutplen = outp;
						}

						retVal = eNoERROR;
					}
				}
				else
				{
					//unable to compress, out of memory
					if (pOutplen)
					{
						*pOutplen = 0;
					}
					if (pMask)
					{
						*pMask	  = 0;
					}
					retVal = eINSUFFICIENTMEMORY;
				}
			}
			else if (maxoutlen>0)
			{
				//this means len is zero
				//we are just done
				if (pOutplen)
				{
					*pOutplen = 0;
				}
				if (pMask)
				{
					*pMask	  = 0;
				}

				result[0] = 0;//eof
				retVal = eNoERROR;
			}
			else
			{
				//this means we have insufficient memory
				if (pOutplen)
				{
					*pOutplen = 0;
				}
				if (pMask)
				{
					*pMask	  = 0;
				}
				retVal = eINSUFFICIENTMEMORY;
			}
			return retVal;
		}

		eErrorCode Deflate(size_t len, T *buffer, size_t outlen, TDataType *result)
		{
			eErrorCode retVal;
			bool MessageDecoded=false;
			if (len&&outlen)
			{
				static const T		THighestBit			= ~static_cast<T>(static_cast<T>(~(static_cast<T>(0)))>>1);
				static const size_t TMaxShifts			= sizeof(T)*8;
				static const size_t TAccTypeMaxShifts	= sizeof(TAccType)*8;
				static const size_t TNormalizationShifts= TAccTypeMaxShifts-TMaxShifts;
				size_t index							= 0;
				size_t outp								= 0;
				result[outp]							= 0;
				size_t shift							= 0;

				m_Tracker.Init(outlen,result);
				size_t symbol							= m_Tracker.GetSymbol(buffer[index]);

				T range;
				T lower;
				m_Tracker.GetSymbolStatistics(symbol,&lower,&range);
				T upper									= lower+range;
				outp									= m_Tracker.UpdateStatistics(symbol);

				//first word flushed, now we have a new range and some left bits
				//acc filled
				TAccType acc	= 0;
				TAccType lacc	= static_cast<TAccType>(lower)<<TNormalizationShifts;
				for (index = 0; index < sizeof(TAccType)/sizeof(T);index++)
				{
					acc <<= TMaxShifts;
					if (index<len)
					{
						acc |= static_cast<TAccType>(buffer[index]);
					}
				}
				//if len >2 missing
				while(static_cast<T>(range)<=THighestBit&&range!=0)
				{
					acc <<= 1;
					lacc <<= 1;
					acc |= (buffer[index]<<(shift++))>>(TMaxShifts-1);
					shift &= (TMaxShifts-1);
					index += (shift==0);
					range <<= 1;
				}
				TAccType longrange;
				longrange = (static_cast<TAccType>(range)<<(TNormalizationShifts-TMaxShifts));
				//decode remaining stream
				do
				{
					TAccType uacc;
					if (longrange!=0)
					{                                        
						TAccType Carry;
						T residue = static_cast<TAccType>(acc-lacc)/(longrange);
						
						symbol = m_Tracker.GetSymbol(residue);
						if (symbol==m_Tracker.EOFIndicator)
						{
							MessageDecoded = true;
							break;
						}
						outp = m_Tracker.UpdateStatistics(symbol);
						m_Tracker.GetSymbolStatistics(symbol,&lower,&range);
						upper = lower+range;
						Carry = lower<upper?upper:(static_cast<TAccType>(1)<<TMaxShifts)+upper;
						uacc = lacc+static_cast<TAccType>(longrange)*Carry;
						lacc = lacc+static_cast<TAccType>(longrange)*lower;
					}
					else
					{
						T residue = (acc-lacc)>>TNormalizationShifts;
						symbol = m_Tracker.GetSymbol(residue);
						if (symbol==m_Tracker.EOFIndicator)
						{
							MessageDecoded = true;
							break;
						}
						outp = m_Tracker.UpdateStatistics(symbol);
						m_Tracker.GetSymbolStatistics(symbol,&lower,&range);
						upper = lower+range;
						uacc = lacc+(static_cast<TAccType>(upper)<<TNormalizationShifts);
						//uacc += lower<upper?0:static_cast<TAccType>(static_cast<TAccType>(1)<<TNormalizationShifts);
						lacc = lacc+(static_cast<TAccType>(lower)<<TNormalizationShifts);
					}


					//range can be large as it is defined as a 2^(2*x) format
					longrange	= static_cast<TAccType>(uacc)-static_cast<TAccType>(lacc);
					range		= longrange>>TNormalizationShifts;
					//range is smaller or equal than sizeof(T)*8
					if (index<len)
					{
						if (range)
						{
							while(static_cast<T>(range)<=THighestBit&&range!=0)
							{
								lacc <<= 1;
								acc <<= 1;
								acc |= (buffer[index]<<(shift++))>>(TMaxShifts-1);
								shift &= (TMaxShifts-1);
								index += (shift==0);
								range <<= 1;
								longrange <<= 1;
							}
						}
						else
						{
							for (size_t i=0;i<TMaxShifts;i++)
							{
								lacc <<= 1;
								acc <<= 1;
								acc |= (buffer[index]<<(shift++))>>(TMaxShifts-1);
								shift &= (TMaxShifts-1);
								index += (shift==0);
								range <<= 1;
								longrange <<= 1;
							}
						}
					}
					else
					{
						if (range)
						{
							while(static_cast<T>(range)<=THighestBit&&range!=0)
							{
								lacc <<= 1;
								acc <<= 1;
								//acc |= (buffer[index]<<(shift++))>>(TMaxShifts-1);
								shift++;
								shift &= (TMaxShifts-1);
								range <<= 1;
								longrange <<= 1;
							}
						}
						else
						{
							for (size_t i=0;i<TMaxShifts;i++)
							{
								lacc <<= 1;
								acc <<= 1;
								//acc |= (buffer[index]<<(shift++))>>(TMaxShifts-1);
								shift++;
								shift &= (TMaxShifts-1);
								range <<= 1;
								longrange <<= 1;
							}
						}
					}
					range = longrange>>TNormalizationShifts;
					longrange >>= TMaxShifts;
				}while(outp<outlen&&((index<len)||((index==len)&&longrange)));


				/*if ((index==len)&&longrange)
				{
					retVal = eDECODINGINCOMPLETE;
				}
				else*/ if (MessageDecoded == true)
				{
					retVal = eNoERROR;
				}
				else if (outp>=outlen)
				{
					retVal = eINSUFFICIENTMEMORY;
				}
				else
				{
					//index<len$
					retVal = eDECODINGINCOMPLETE;
				}
			}
			else if (outlen>0)
			{
				result[0]=0;//eof
				retVal = eNoERROR; 
			}
			else
			{
				//outlen is zero
				retVal = eINSUFFICIENTMEMORY;
			}
			return retVal;
		}
	};
} //end of namespace
#endif //_ST_ADAPTIVE_ARITHMETIC_CODEC_