!*  ============================================================================
!*
!*  DATE                       : 2010-11-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : COSH in elemental function
!*
!*  REFERENCE                  : Feature Number 376003
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  This program tests the COSH(x) intrinsic procedure where (-1 <= x <= 1)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module trig

   contains

   real elemental function coshRF(arrayR)
      real(4), intent(in) :: arrayR
      coshRF = cosh(arrayR)
      return
   end function coshRF

   complex elemental function coshCF(arrayC)
      complex(4), intent(in) :: arrayC
      coshCF = cosh(arrayC)
      return
   end function coshCF

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
   R_C = (/ (1.354180694,0.5634214878), (1.236858130,0.2430112213), (1.105148554,0.1035256535), (0.9950041771,0.0000000000E+00), (0.9306698442,-0.2942325473), (0.9902284145,-0.5297510028), (0.9591943622,-0.9205666780) /)
   R = (/ 1.0, 0.75, 0.5, 0.0, -0.5, -0.75, -1.0 /)
   R_R = (/ 1.543080688, 1.294683337, 1.127625942, 1.0, 1.127625942, 1.294683337, 1.543080688 /)

   RES_R = coshRF(R)
   do i = 1, 7
      if (.NOT. precision_r4(RES_R(i), R_R(i))) call zzrc(i)
   end do
   RES_C = coshCF(C)
   do i = 1, 7
      if (.NOT. precision_x8(RES_C(i), R_C(i))) call zzrc(i+7)
   end do

   ASSOCIATE ( a => cosh(1.0), b => cosh( (1.0,0.75) ) )
      if (acosh(a) .NE. 1.0) call zzrc(i+8)
      if (acosh(b) .NE. (1.0,0.75)) call zzrc(i+9)
   END ASSOCIATE

end program main
