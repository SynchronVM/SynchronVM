/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2021 Joel Svensson, Abhiroop Sarkar 				  */
/* 										  */
/* Permission is hereby granted, free of charge, to any person obtaining a copy	  */
/* of this software and associated documentation files (the "Software"), to deal  */
/* in the Software without restriction, including without limitation the rights	  */
/* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell	  */
/* copies of the Software, and to permit persons to whom the Software is	  */
/* furnished to do so, subject to the following conditions:			  */
/* 										  */
/* The above copyright notice and this permission notice shall be included in all */
/* copies or substantial portions of the Software.				  */
/* 										  */
/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR	  */
/* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,	  */
/* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE	  */
/* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER	  */
/* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  */
/* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  */
/* SOFTWARE.									  */
/**********************************************************************************/

#ifndef CS43L22_DEFS_H_
#define CS43L22_DEFS_H_


/* Registers as found in the CS43L22 datasheet */
#define CS43L22_ID_REG             0x01
#define CS43L22_PWR_CTL_1_REG      0x02
#define CS43L22_PWR_CTL_2_REG      0x04
#define CS43L22_CLK_CTL_REG        0x05
#define CS43L22_IF_CTL_1_REG       0x06 /* interface control*/
#define CS43L22_IF_CTL_2_REG       0x07
#define CS43L22_PT_SEL_A_REG       0x08 /* Passthrough A select*/
#define CS43L22_PT_SEL_B_REG       0x09
#define CS43L22_ANALOG_REG         0x0A
#define CS43L22_PT_GANG_REG        0x0C
#define CS43L22_PLAYBACK_CTL_1     0x0D
#define CS43L22_MISC_CTL           0x0E
#define CS43L22_PLAYBACK_CTL_2     0x0F
#define CS43L22_PT_VOL_A           0x14
#define CS43L22_PT_VOL_B           0x15
#define CS43L22_PCMA_VOL           0x1A
#define CS43L22_PCMB_VOL           0x1B
#define CS43L22_BEEP_FRQ_ONTIME    0x1C
#define CS43L22_BEEP_VOL_OFFTIME   0x1D
#define CS43L22_BEEP_TONE          0x1E
#define CS43L22_TONE_CTL           0x1F /* bass and trebble */
#define CS43L22_MASTER_VOL_A       0x20 /* master volume */
#define CS43L22_MASTER_VOL_B       0x21 
#define CS43L22_PHONES_VOL_A       0x22 /* Headphones volume */
#define CS43L22_PHONES_VOL_B       0x23
#define CS43L22_SPEAKER_VOL_A      0x24
#define CS43L22_SPEAKER_VOL_B      0x25
#define CS43L22_MIX_SWAP           0x26
#define CS43L22_LIMIT_CTL_1        0x27
#define CS43L22_LIMIT_CTL_2        0x28
#define CS43L22_LIMIT_ATTACK       0x29
#define CS43L22_OVERFLOW_CLKSTAT   0x2E
#define CS43L22_BAT_COMPENSATION   0x2F
#define CS43L22_BAT_LEVEL          0x30
#define CS43L22_SPEAKER_STATUS     0x31
#define CS43L22_CHARGE_PUMP_FRQ    0x34 /* very low level thing? */


/* Access data fields */

#define CS43L22_CHIP_ID(X) ((X) >> 3)
#define CS43L22_REV_ID(X)  ((X) & 0x7)


#endif
