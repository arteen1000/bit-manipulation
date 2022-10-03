/* 
 * CS:APP Data Lab 
 * 
 * <Arteen Abrishami 205-577-156>
 * 
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.  
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:
 
  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code 
  must conform to the following style:
 
  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>
    
  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.

 
  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers. // 4 bytes (Tmax is ... 2147483647)
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting if the shift amount
     is less than 0 or greater than 31.     // very important assumptions


EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;       // shifts over 1 x many positions, gets 1 times 2 ^ x (starts at position 0) and then + 1
  }
    // shift integer << shift amount

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }


NOTES:
  1. Our checker requires that you do NOT define a variable after 
     a statement that does not define a variable.

     For example, this is NOT allowed:

     int illegal_function_for_this_lab(int x, int y) {
      // this statement doesn't define a variable
      x = x + y + 1;
      
      // The checker for this lab does NOT allow the following statement,
      // because this variable definition comes after a statement 
      // that doesn't define a variable
      int z;

      return 0;
     }
     
  2. VERY IMPORTANT: Use the dlc (data lab checker) compiler (described in the handout)
     to check the legality of your solutions.
  3. Each function has a maximum number of operations (integer, logical,
     or comparison) that you are allowed to use for your implementation
     of the function.  The max operator count is checked by dlc.
     Note that assignment ('=') is not counted; you may use as many of
     these as you want without penalty.
  4. Use the btest to check your functions for correctness.
  5. The maximum number of ops for each function is given in the
     header comment for each function. 

/*
 * STEP 2: Modify the following functions according the coding rules.
 * 
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the btest to verify that your solutions produce 
 *      the correct answers.
 */


#endif
/* Copyright (C) 1991-2012 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* We do support the IEC 559 math functionality, real and complex.  */
/* wchar_t uses ISO/IEC 10646 (2nd ed., published 2011-03-15) /
   Unicode 6.0.  */
/* We do not support C11 <threads.h>.  */
/* 
 * TMax - return maximum two's complement integer 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 4
 *   Rating: 1
 */

int tmax(void) {
/* exploit features of shift to get tmin and that of tmin (1000....0000) to get tmax as the negation of the individual bits*/

   return ~(1 << 31);

} 

/*
 * isZero - returns 1 if x == 0, and 0 otherwise 
 *   Examples: isZero(5) = 0, isZero(0) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 2
 *   Rating: 1
 */

int isZero(int x) {
/* exploit features of logical shift where !0x00 = 0x01 (true) and !0x## = 0x00 (false) */

  return !x;
}

/* 
 * bitXor - x^y using only ~ and & 
 *   Example: bitXor(4, 5) = 1
 *   Legal ops: ~ &
 *   Max ops: 14
 *   Rating: 1
 */

int bitXor(int x, int y) {
/* ^ is the equivalent of a symmetric difference (0 when the same, 1 when not) - tells us 1 in places where the bits are different
   the negation of the sameness of 1s, where sameness is determined by &, yields us the difference of 1s: ~(x&y)
   the negation of the sameness of 0s, where we negate x and y in order to make the 0s -> 1s in order to be picked up
   by & (where & = 1 when both components are 1), yields us the difference of 0s ~(~x&~y)

   the places where those differences are the same (determined by 1) can be found by &ing the two sides
   determining the symmetric difference (where there is a difference between 0s and 1s)
   
*/

    return (~(x&y)) & (~(~x&~y));
}

/* 
 * isNotEqual - return 0 if x == y, and 1 otherwise 
 *   Examples: isNotEqual(5,5) = 0, isNotEqual(4,5) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 2
 */

int isNotEqual(int x, int y) {
   /* the !! yields us 0x00->0x00 and 0x##->0x01 where nonzeroes are true and 0s are false
      if the numbers are equal then the ^ will give us 0 as there will be no place with any
      difference (what ^ determines - places of difference)
      
      we use the !! to yield 0x00 for that result where they are equal and 0x01 otherwise (where they are not equal) */

   return !!(x^y);
}

/* 
 * sign - return 1 if positive, 0 if zero, and -1 if negative
 *  Examples: sign(130) = 1
 *            sign(-23) = -1
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 10
 *  Rating: 2
 */

int sign(int x) {
   /* using the properties of !!, we transform 0x00->0x00 and 0x## (nonzeroes) -> 0x01 
      in order to preserve the signnedness, we | this with the MSB shifted to the rightmost position (resulting in arithmetic fill in of 0s or 1s)
      
      this yields:
      0x01 for nonzeroes where the sign (MSB) is 0, but are nonzero themselves  (all but the LSB stay 0 with the comparison operator | )
      0x00 for zero
      0xFFFFFFFF for negative nonzeroes (MSB = 1)
       */

    return (!!x) | (x >> 31);
}

/* 
 * conditional - same as x ? y : z 
 *   Example: conditional(2,4,5) = 4    // condition? true_output : false_output
 *   Legal ops: ! ~ & ^ | + << >>       // true means 1, false means 0 , !!
 *   Max ops: 16                  // long story short, if x = 1 (nonzero) -> return y , if x = 0 -> return z
 *  
 *   Rating: 3
 */

int conditional(int x, int y, int z) {
   /*  exploited bit operations in order to create a mask of all 1s or all 0s in order to superimpose my true_output or false_output based on
      the characteristics of the mask */
   
    x = ( (~(!!x)) + 1);         // !!x-> 0x00 for 0 and 0x01 for nonzeroes -> ~ + 1 gives us all bits on for nonzeroes (-1), all bits off for 0 (0)
                                       // aka all bits off (0) when condition false and all bits on (-1) when condition true
    return (x & y) | (~x & z);      // works as such: if x is 111111111 -> returns y, if x is 000000 -> returns z
                                          // because - if x were 111111, would act ask a mask for y, and be 00000 (an anti-mask for z) and vice versa
}

/* 
 * replaceByte(x,n,c) - Replace byte n in x with c
 *   Bytes numbered from 0 (LSB) to 3 (MSB)
 *   Examples: replaceByte(0x12345678,1,0xab) = 0x1234ab78
 *   You can assume 0 <= n <= 3 and 0 <= c <= 255
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 10
 *   Rating: 3
 */

int replaceByte(int x, int n, int c) {
/* exploits properties of masking and shifting in order to create a situation where the original bytes not numbered n may be preserved while
   the nth byte is effectively negated in x and transformed into c */
   
    int mask = 0xff;             // one bit mask
    n <<= 3;                    // equivalent of *= 8 (bits per byte), transforms into a format to shift by the argument n number of bytes, as shifting occurs by bit
    mask <<= n;                 // must occur after integer declaration
    c = c << n;                  // shifts c over to the desired byte position to copy over
    
    return (~mask & x) | c;         // creates an anti-mask (where the original mask position is 0s and else where is 1s to copy x into all positions where the c 
                                    // byte is not to ocupy and allow the c byte (which has 0s elsewhere) to  copy itself into that position with the | operator
}

/* 
 * isAsciiDigit - return 1 if 0x30 <= x <= 0x39 (ASCII codes for characters '0' to '9')
 *   Example: isAsciiDigit(0x35) = 1.
 *            isAsciiDigit(0x3a) = 0.
 *            isAsciiDigit(0x05) = 0.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 3
 */

int isAsciiDigit(int x) {
/* Hypothesis: 0x30 <= x <= 0x39 is the same as x - 0x30 >= 0x00 and also 0x39 - x >= 0
   making sure theese two conditions are satisfied allows us to return the desired result of 
   1 using the logic gate ! when both conditions are masked by 0, otherwise it would return 0 */

    int condition1 = (x + (~0x2f)) >> 31;      // to save an operation: instead of ~0x30 + 1 for the negation of 0x30, I found the bit pattern to be 0xffffffd0, which is ~0x2f
    int condition2 = (0x39 + (~x + 1)) >> 31;      // operation functions by shifting the substraction of 0x39 - x 's MSB over to the LSB, causing an arithmetic shift

                                                   // what this does is create a mask of all 0s if the result is positive (as we wish it to be) and a mask of all 1s if the result is negative 
                                                                     // in which case it would not be an ascii digit
    return !condition1 & !condition2;           // when both are occupied by a mask of 0, will return 1, otherwise will return 0
}

/* 
 * subtractionOK - Determine if can compute x-y without overflow
 *   Example: subtractionOK(0x80000000,0x80000000) = 1,
 *            subtractionOK(0x80000000,0x70000000) = 0, 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */

int subtractionOK(int x, int y) {
/* Hypothesis: x - y must act as follows for subtractionOK to output 0 (false case):
         x = pos. y = neg. result (z) = neg.
         x = neg. y = pos. result (z) = pos.
      meaning that x and y must have opposite signs (condition1) and x and z must also have opposite signs (condition2)
 
    */
                                       // the MSBs are all that matter, >> 31 one time in the return
    int z = (x + (~y + 1));            // MSB 1 if neg. 0 if pos.
    int condition1 = (x^y);            // symmetric difference -> 0 if both pos. or neg., 1 if different signs
    int condition2 = (x^z);             // -> 0 if both pos. or neg., 1 if different signs                        
    
    return !((condition1 & condition2) >> 31);        // we wish for the MSB of condition1 and condition2 to both be 1 if we are to output 0
                                                      // & determines this, shifting to LSB creates a mask of 1111... if conditions satisfied (for output 0)
                                                      // mask of 0000.... if conditions satisfied (for output 1)
                                                      // using the logical shift, we make this possible, where ! (-1) = 0x00 and the !0x00 = 0x01
}

// below are extra credits (4 pts in total)
// 2 points each (1 correctness pt + 1 performance pt)
/* 
 * leastBitPos - return a mask that marks the position of the
 *               least significant 1 bit. If x == 0, return 0
 *   Example: leastBitPos(96) = 0x20
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 */

int leastBitPos(int x) {
   /* exploit negation to pass back the lowest set bit */

  return x & (~x + 1);     // ~x gives us back 0s in place of 1s and vice versa. adding a 1 allows it to traverse into the first 0 location
                           // effectively passing the intermediate 1s, and getting placed into the first 0 pos., which is the first bit set in x
                           // &ing the two yields us a combination where only the one bit (the first bit) is set as the 1s are now 0s in the (~x + 1) and would not
                           // be included in the &, leaving only the +1 that was added, going into the first 0 slot (the space of the leastBitPos)
}

/* 
 * greatestBitPos - return a mask that marks the position of the
 *               most significant 1 bit. If x == 0, return 0
 *   Example: greatestBitPos(96) = 0x40
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 70
 */

int greatestBitPos(int x) {
   /* create exclusionary cases for negatives and positives and exploit shift feature in order to single out MSB and return it with an or case*/ 

    int isPositive = (~(!!(~(x>>31) & x))+1);      // gives all 0s if x is 0 or negative, all 1s otherwise (positive)
    int isNegative = (x >> 31);                     // all 1s if negative, 0s if 0 or positive
    int negativeReturn = (1 << 31);                 // what we wish to return when negative (Tmin)
    
    x |= x >> 1;
    x |= x >> 2;
    x |= x >> 4;
    x |= x >> 8;
    x |= x >> 16;               // fills in all to the right of MSB with 1s

    x >>= 1;
    x += 1;         // similar to leastBitPos but trickier -> leaves MSB if pos. and 0 if neg.
    
    return (isPositive & x) | (isNegative & (negativeReturn));
    
    // LHS returns gBP if positive and does nothing if negative or zero and the RHS does the same for a negative return (Two's Complement)
}

