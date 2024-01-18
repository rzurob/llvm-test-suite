! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : underflowCtrl3.f
!*
!*  PROGRAMMER                 : Nancy Wang
!*  DATE                       : Nov. 15 2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : ieee_support_underflow_control(X) 
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*  REFERENCE                  : Feature Number 289080
!*
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                :    
!*  1. test if ieee_support_underflow_control(X) returns false when passing 
!*     a literal constant of real type argument with kind equal to 4,8,16  
!*  2. test if ieee_support_underflow_control(X) returns false when passing 
!*     a named constant of real type argument with kind equal to 4,8,16 
!*  3. test if ieee_support_underflow_control(X) returns false when passing
!*     a named constant of real type array argument with kind equal to 4,8,16 
!*
!23456789012345678901234567890123456789012345678901234567890123456789012   
      module m
         integer,parameter  :: k4=4
         integer,parameter  :: k8=8
         integer,parameter  :: k16=16

         real(4):: a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,  &
                   a14,a15,a16,a17,a18,a19,a20,a21
         parameter (a1=-2.5,     a2=-2.5_4,     a3=-2.5_k4,     &
                    a4=-.2,      a5=-.2_4,      a6=-.2_k4,      &
                    a7=-2.,      a8=-2._4,      a9=-2._k4,      &
                    a10=-2.5E02, a11=-2.5E02_4, a12=-2.5E02_k4, &
                    a13=-2.E-1,  a14=-2.E-1_4,  a15=-2.E-1_k4,  &
                    a16=+2E+1,   a17=+2E+1_4,   a18=+2E+1_k4,   &
                    a19=-.2E+1,  a20=-.2E+1_4,  a21=-.2E+1_k4   ) 

         real(8):: b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,  &
                   b14,b15,b16,b17,b18,b19,b20,b21
         parameter (b1=-2.5,     b2=-2.5_8,     b3=-2.5_k8,     &
                    b4=-.2,      b5=-.2_8,      b6=-.2_k8,      &
                    b7=-2.,      b8=-2._8,      b9=-2._k8,      &
                    b10=-2.5D02, b11=-2.5E02_8, b12=-2.5E02_k8, &
                    b13=-2.E-1,  b14=-2.E-1_8,  b15=-2.E-1_k8,  &
                    b16=+2E+1,   b17=+2E+1_8,   b18=+2E+1_k8,   &
                    b19=-.2D+1,  b20=-.2E+1_8,  b21=-.2E+1_k8   )

         real(16):: c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13, &
                   c14,c15,c16,c17,c18,c19,c20,c21
         parameter (c1=-2.5,     c2=-2.5_16,    c3=-2.5_k16,    &
                    c4=-.2,      c5=-.2_16,     c6=-.2_k16,     &
                    c7=-2.,      c8=-2._16,     c9=-2._k16,     &
                    c10=-2.5Q02, c11=-2.5E02_16,c12=-2.5E02_k16,&
                    c13=-2.E-1,  c14=-2.E-1_16, c15=-2.E-1_k16, &
                    c16=+2E+1,   c17=+2E+1_16,  c18=+2E+1_k16,  &
                    c19=-.2Q+1,  c20=-.2E+1_16, c21=-.2E+1_k16  )

         real(4),dimension(20)  :: A
         real(8),dimension(20)  :: B
         real(16),dimension(20) :: C   
         real(4),dimension(5,4) :: AA      
         real(8),dimension(5,4) :: BB
         real(16),dimension(5,4):: CC

      end module m

      program underflowCtrl3
         use,intrinsic :: ieee_arithmetic
         use m 
         implicit none


         A=(/a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13, &
             a14,a15,a16,a17,a18,a19,a20/)
         B=(/b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13, &
             b14,b15,b16,b17,b18,b19,b20/)
         C=(/c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13, &
             c14,c15,c16,c17,c18,c19,c20/)
         AA=reshape(A,(/5,4/))
         BB=reshape(B,(/5,4/))
         CC=reshape(C,(/5,4/))

!        pass a literal constant of real type with kind=4  
         if (ieee_support_underflow_control(-2.5))         error stop 101_4
         if (ieee_support_underflow_control(-2.5_4))       error stop 102_4
         if (ieee_support_underflow_control(-2.5_k4))      error stop 103_4
         if (ieee_support_underflow_control(-.2))          error stop 104_4
         if (ieee_support_underflow_control(-.2_4))        error stop 105_4
         if (ieee_support_underflow_control(-.2_k4))       error stop 106_4
         if (ieee_support_underflow_control(-2.))          error stop 107_4
         if (ieee_support_underflow_control(-2._4))        error stop 108_4
         if (ieee_support_underflow_control(-2._k4))       error stop 109_4
         if (ieee_support_underflow_control(-2.5E02))      error stop 110_4
         if (ieee_support_underflow_control(-2.5E02_4))    error stop 111_4
         if (ieee_support_underflow_control(-2.5E02_k4))   error stop 112_4
         if (ieee_support_underflow_control(-2.E-1))       error stop 113_4
         if (ieee_support_underflow_control(-2.E-1_4))     error stop 114_4
         if (ieee_support_underflow_control(-2.E-1_k4))    error stop 115_4
         if (ieee_support_underflow_control(+2E+1))        error stop 116_4
         if (ieee_support_underflow_control(+2E+1_4))      error stop 117_4
         if (ieee_support_underflow_control(+2E+1_k4))     error stop 118_4
         if (ieee_support_underflow_control(-.2E+1))       error stop 119_4
         if (ieee_support_underflow_control(-.2E+1_4))     error stop 120_4
         if (ieee_support_underflow_control(-.2E+1_k4))    error stop 121_4

!        pass a literal constant of real type with kind=8
         if (ieee_support_underflow_control(-2.5_8))       error stop 122_4
         if (ieee_support_underflow_control(-2.5_k8))      error stop 123_4
         if (ieee_support_underflow_control(-.2_8))        error stop 124_4
         if (ieee_support_underflow_control(-.2_k8))       error stop 125_4
         if (ieee_support_underflow_control(-2._8))        error stop 126_4
         if (ieee_support_underflow_control(-2._k8))       error stop 127_4
         if (ieee_support_underflow_control(-2.5D26))      error stop 128_4
         if (ieee_support_underflow_control(-2.5E26_8))    error stop 129_4
         if (ieee_support_underflow_control(-2.5E26_k8))   error stop 130_4
         if (ieee_support_underflow_control(-2.D-15))      error stop 131_4
         if (ieee_support_underflow_control(-2.E-15_8))    error stop 132_4
         if (ieee_support_underflow_control(-2.E-15_k8))   error stop 133_4
         if (ieee_support_underflow_control(+2D+34))       error stop 134_4
         if (ieee_support_underflow_control(+2E+34_8))     error stop 135_4
         if (ieee_support_underflow_control(+2E+34_k8))    error stop 136_4
         if (ieee_support_underflow_control(-.2D+13))      error stop 137_4
         if (ieee_support_underflow_control(-.2E+13_8))    error stop 138_4
         if (ieee_support_underflow_control(-.2E+13_k8))   error stop 139_4

!        pass a literal constant of real type with kind=16
         if (ieee_support_underflow_control(-2.5_16))      error stop 140_4
         if (ieee_support_underflow_control(-2.5_k16))     error stop 141_4
         if (ieee_support_underflow_control(-.2_16))       error stop 142_4
         if (ieee_support_underflow_control(-.2_k16))      error stop 143_4
         if (ieee_support_underflow_control(-2._16))       error stop 144_4
         if (ieee_support_underflow_control(-2._k16))      error stop 145_4
         if (ieee_support_underflow_control(-2.5Q263))     error stop 146_4
         if (ieee_support_underflow_control(-2.5E263_16))  error stop 147_4
         if (ieee_support_underflow_control(-2.5E263_k16)) error stop 148_4
         if (ieee_support_underflow_control(-2.Q-157))     error stop 149_4
         if (ieee_support_underflow_control(-2.E-157_16))  error stop 150_4
         if (ieee_support_underflow_control(-2.E-157_k16)) error stop 151_4
         if (ieee_support_underflow_control(+2Q+301))      error stop 152_4
         if (ieee_support_underflow_control(+2E+301_16))   error stop 153_4
         if (ieee_support_underflow_control(+2E+301_k16))  error stop 154_4
         if (ieee_support_underflow_control(-.2Q+136))     error stop 155_4
         if (ieee_support_underflow_control(-.2E+136_16))  error stop 156_4
         if (ieee_support_underflow_control(-.2E+136_k16)) error stop 157_4

!        pass a named constant of real type with kind=4 
         if (ieee_support_underflow_control(a1))           error stop 158_4
         if (ieee_support_underflow_control(a2))           error stop 159_4
         if (ieee_support_underflow_control(a3))           error stop 160_4
         if (ieee_support_underflow_control(a4))           error stop 161_4
         if (ieee_support_underflow_control(a5))           error stop 162_4
         if (ieee_support_underflow_control(a6))           error stop 163_4
         if (ieee_support_underflow_control(a7))           error stop 164_4
         if (ieee_support_underflow_control(a8))           error stop 165_4
         if (ieee_support_underflow_control(a9))           error stop 166_4
         if (ieee_support_underflow_control(a10))          error stop 167_4
         if (ieee_support_underflow_control(a11))          error stop 168_4
         if (ieee_support_underflow_control(a12))          error stop 169_4
         if (ieee_support_underflow_control(a13))          error stop 170_4
         if (ieee_support_underflow_control(a14))          error stop 171_4
         if (ieee_support_underflow_control(a15))          error stop 172_4
         if (ieee_support_underflow_control(a16))          error stop 173_4
         if (ieee_support_underflow_control(a17))          error stop 174_4
         if (ieee_support_underflow_control(a18))          error stop 175_4
         if (ieee_support_underflow_control(a19))          error stop 176_4
         if (ieee_support_underflow_control(a20))          error stop 177_4
         if (ieee_support_underflow_control(a21))          error stop 178_4 
    
!        pass a named constant of real type with kind=8
         if (ieee_support_underflow_control(b1))           error stop 179_4
         if (ieee_support_underflow_control(b2))           error stop 180_4
         if (ieee_support_underflow_control(b3))           error stop 181_4
         if (ieee_support_underflow_control(b4))           error stop 182_4
         if (ieee_support_underflow_control(b5))           error stop 183_4
         if (ieee_support_underflow_control(b6))           error stop 184_4
         if (ieee_support_underflow_control(b7))           error stop 185_4
         if (ieee_support_underflow_control(b8))           error stop 186_4
         if (ieee_support_underflow_control(b9))           error stop 187_4
         if (ieee_support_underflow_control(b10))          error stop 188_4
         if (ieee_support_underflow_control(b11))          error stop 189_4
         if (ieee_support_underflow_control(b12))          error stop 190_4
         if (ieee_support_underflow_control(b13))          error stop 191_4
         if (ieee_support_underflow_control(b14))          error stop 192_4
         if (ieee_support_underflow_control(b15))          error stop 193_4
         if (ieee_support_underflow_control(b16))          error stop 194_4
         if (ieee_support_underflow_control(b17))          error stop 195_4
         if (ieee_support_underflow_control(b18))          error stop 196_4
         if (ieee_support_underflow_control(b19))          error stop 197_4
         if (ieee_support_underflow_control(b20))          error stop 198_4
         if (ieee_support_underflow_control(b21))          error stop 199_4

!        pass a named constant of real type with kind=16
         if (ieee_support_underflow_control(c1))           error stop 200_4
         if (ieee_support_underflow_control(c2))           error stop 201_4
         if (ieee_support_underflow_control(c3))           error stop 202_4
         if (ieee_support_underflow_control(c4))           error stop 203_4
         if (ieee_support_underflow_control(c5))           error stop 204_4
         if (ieee_support_underflow_control(c6))           error stop 205_4
         if (ieee_support_underflow_control(c7))           error stop 206_4
         if (ieee_support_underflow_control(c8))           error stop 207_4
         if (ieee_support_underflow_control(c9))           error stop 208_4
         if (ieee_support_underflow_control(c10))          error stop 209_4
         if (ieee_support_underflow_control(c11))          error stop 210_4
         if (ieee_support_underflow_control(c12))          error stop 211_4
         if (ieee_support_underflow_control(c13))          error stop 212_4
         if (ieee_support_underflow_control(c14))          error stop 213_4
         if (ieee_support_underflow_control(c15))          error stop 214_4
         if (ieee_support_underflow_control(c16))          error stop 215_4
         if (ieee_support_underflow_control(c17))          error stop 216_4
         if (ieee_support_underflow_control(c18))          error stop 217_4
         if (ieee_support_underflow_control(c19))          error stop 218_4
         if (ieee_support_underflow_control(c20))          error stop 219_4
         if (ieee_support_underflow_control(c21))          error stop 220_4

!        pass a named constant of real type array with kind equal to 4,8,16
         if (ieee_support_underflow_control(A))            error stop 221_4
         if (ieee_support_underflow_control(B))            error stop 222_4
         if (ieee_support_underflow_control(C))            error stop 223_4
         if (ieee_support_underflow_control(AA))           error stop 224_4
         if (ieee_support_underflow_control(BB))           error stop 225_4
         if (ieee_support_underflow_control(CC))           error stop 226_4 

       end program
