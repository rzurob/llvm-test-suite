!*  ============================================================================
!*
!*  DATE                       : 2010-11-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : TAN in elemental function
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
!*  This program tests the TAN(x) intrinsic procedure where (-1 <= x <= 1)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module trig

   contains

   real elemental function tanRF(arrayR)
      real(4), intent(in) :: arrayR
      tanRF = tan(arrayR)
      return
   end function tanRF

   complex elemental function tanCF(arrayC)
      complex(4), intent(in) :: arrayC
      tanCF = tan(arrayC)
      return
   end function tanCF

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
   R_C = (/ (0.8068774343,1.042830706), (0.7940559387,0.5068081617), (0.5189861655,0.2533358634), (0.0000000000E+00,0.9966799617E-01), (-0.3579268456,0.6420623064), (-0.4489912689,0.8571619391), (-0.3378622234,1.093206048) /)
   R = (/ 1.0, 0.75, 0.5, 0.0, -0.5, -0.75, -1.0 /)
   R_R = (/ 1.557407737, 0.9315964580, 0.5463024974, 0.0, -0.5463024974, -0.9315964580, -1.557407737 /)

   RES_R = tanRF(R)
   do i = 1, 7
      if (.NOT. precision_r4(RES_R(i), R_R(i))) call zzrc(i)
   end do
   RES_C = tanCF(C)
   do i = 1, 7
      if (.NOT. precision_x8(RES_C(i), R_C(i))) call zzrc(i+7)
   end do

   ASSOCIATE ( a => tan(1.0), b => tan( (1.0,0.75) ) )
      if (atan(a) .NE. 1.0) call zzrc(i+8)
      if (atan(b) .NE. (1.0,0.75)) call zzrc(i+9)
   END ASSOCIATE

end program main