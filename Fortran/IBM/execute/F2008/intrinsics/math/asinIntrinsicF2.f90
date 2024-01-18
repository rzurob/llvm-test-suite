!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : asinIntrinsicF2.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2010-11-16
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ASIN in elemental function
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
!*  This program tests the ASIN(x) intrinsic procedure where (-1 <= x <= 1)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module trig

   contains

   real elemental function asinRF(arrayR)
      real(4), intent(in) :: arrayR
      asinRF = asin(arrayR)
      return
   end function asinRF

   complex elemental function asinCF(arrayC)
      complex(4), intent(in) :: arrayC
      asinCF = asin(arrayC)
      return
   end function asinCF

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
   R_C = (/ (0.8959074616,0.7328577042), (0.7649595141,0.4047130644), (0.5090856552,0.2270882726), (0.0000000000E+00,0.9983407706E-01), (-0.4304306805,0.6197743416), (-0.6074215174,0.7731347680), (-0.7031000257,1.002976656) /)
   R = (/ 1.0, 0.75, 0.5, 0.0, -0.5, -0.75, -1.0 /)
   R_R = (/ 1.570796371, 0.8480620980, 0.5235987902, 0.0, -0.5235987902, -0.8480620980, -1.570796371 /)

   RES_R = asinRF(R)
   do i = 1, 7
      if (.NOT. precision_r4(RES_R(i), R_R(i))) call zzrc(i)
   end do
   RES_C = asinCF(C)
   do i = 1, 7
      if (.NOT. precision_x8(RES_C(i), R_C(i))) call zzrc(i+7)
   end do

   ASSOCIATE ( a => asin(1.0), b => asin( (1.5,0.75) ) )
      if (sin(a) .NE. 1.0) call zzrc(i+8)
      if (.NOT. precision_x8(sin(b), (1.5,0.75))) call zzrc(i+9)
   END ASSOCIATE

end program main
