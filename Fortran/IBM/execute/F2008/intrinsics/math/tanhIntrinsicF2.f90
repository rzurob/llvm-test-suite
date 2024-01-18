!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : tanhIntrinsicF2.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2010-11-16
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : TANH in elemental function
!*
!*  REFERENCE                  : Feature Number 376003
!*
!*  DRIVER StanhZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  This program tests the TANH(x) intrinsic procedure where (-1 <= x <= 1)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module trig

   contains

   real elemental function tanhRF(arrayR)
      real(4), intent(in) :: arrayR
      tanhRF = tanh(arrayR)
      return
   end function tanhRF

   complex elemental function tanhCF(arrayC)
      complex(4), intent(in) :: arrayC
      tanhCF = tanh(arrayC)
      return
   end function tanhCF

end module trig

program main

   use trig

   implicit none

   integer i
   double precision D
   real(4), dimension(7) :: R, R_R, RES_R
   complex(4), dimension(7) :: C, R_C, RES_C

   interface
      logical(4) function precision_r4(a, b)
         real(4) a, b
      end function
      logical(4) function precision_x8(a, b)
        complex(4) a, b
      end function
   end interface

   C = (/ (1.0,0.5),   (0.75,0.3),   (0.5,0.2),    (0.0,0.1), (-0.5,0.6),    (-0.75,0.7),  (-1.0,0.9) /)
   R_C = (/ (0.8429661989,0.1955773085), (0.6700598598,0.1776865274), (0.4769211113,0.1580340713), (0.0000000000E+00,0.1003346741), (-0.6167616248,0.4891468287), (-0.8441559672,0.3906829953), (-1.025987864,0.2754878104) /)
   R = (/ 1.0, 0.75, 0.5, 0.0, -0.5, -0.75, -1.0 /)
   R_R = (/ 0.7615941763, 0.6351489425, 0.4621171653, 0.0, -0.4621171653, -0.6351489425, -0.7615941763 /)

   RES_R = tanhRF(R)
   do i = 1, 7
      if (.NOT. precision_r4(RES_R(i), R_R(i))) call zzrc(i)
   end do
   RES_C = tanhCF(C)
   do i = 1, 7
      if (.NOT. precision_x8(RES_C(i), R_C(i))) call zzrc(i+7)
   end do

   ASSOCIATE ( a => tanh(1.0), b => tanh( (1.0,0.5) ) )
      if (atanh(a) .NE. 1.0) call zzrc(i+8)
      if (.NOT. precision_x8(atanh(b), (1.0,0.5))) call zzrc(i+9)
   END ASSOCIATE

end program main
