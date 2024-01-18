!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : sinhIntrinsicF2.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2010-11-16
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : SINH in elemental function
!*
!*  REFERENCE                  : Feature Number 376003
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  This program tests the SINH(x) intrinsic procedure where (-1 <= x <= 1)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module trig

   contains

   real elemental function sinhRF(arrayR)
      real(4), intent(in) :: arrayR
      sinhRF = sinh(arrayR)
      return
   end function sinhRF

   complex elemental function sinhCF(arrayC)
      complex(4), intent(in) :: arrayC
      sinhCF = sinh(arrayC)
      return
   end function sinhCF

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
   R_C = (/ (1.031336069,0.7397922873), (0.7855891585,0.3826050758), (0.5107080936,0.2240246981), (0.0000000000E+00,0.9983342141E-01), (-0.4300785065,0.6367055178), (-0.6289425492,0.8340578675), (-0.7305167913,1.208736539) /)
   R = (/ 1.0, 0.75, 0.5, 0.0, -0.5, -0.75, -1.0 /)
   R_R = (/ 1.175201178, 0.8223167062, 0.5210952759, 0.0, -0.5210952759, -0.8223167062, -1.175201178 /)

   RES_R = sinhRF(R)
   do i = 1, 7
      if (.NOT. precision_r4(RES_R(i), R_R(i))) call zzrc(i)
   end do
   RES_C = sinhCF(C)
   do i = 1, 7
      if (.NOT. precision_x8(RES_C(i), R_C(i))) call zzrc(i+7)
   end do

   ASSOCIATE ( a => sinh(1.0), b => sinh( (1.0,0.75) ) )
      if (asinh(a) .NE. 1.0) call zzrc(i+8)
      if (.NOT. precision_x8(asinh(b), (1.0,0.75))) call zzrc(i+9)
   END ASSOCIATE

end program main
