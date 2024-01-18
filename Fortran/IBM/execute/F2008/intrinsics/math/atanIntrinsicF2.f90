!*  ============================================================================
!*
!*  TEST CASE NAME             : atanIntrinsicF2.f
!*
!*  DATE                       : 2010-11-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ATAN in elemental function
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
!*  This program tests the ATAN(x) intrinsic procedure where (-1 <= x <= 1)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module trig

   contains

   real elemental function atanRF(arrayR)
      real(4), intent(in) :: arrayR
      atanRF = atan(arrayR)
      return
   end function atanRF

   complex elemental function atanCF(arrayC)
      complex(4), intent(in) :: arrayC
      atanCF = atan(arrayC)
      return
   end function atanCF

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
   R_C = (/ (0.8475756645,0.2388778627), (0.6715728045,0.1902181208), (0.4766952097,0.1603155881), (0.0000000000E+00,0.1003353521), (-0.5994701385,0.4811956584), (-0.8028910160,0.4165106714), (-0.9778028131,0.3795693815) /)
   R = (/ 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0 /)
   R_R = (/ 1.471127629, 1.520837903, 1.537475348, 1.545801520, 1.550799012, 1.554131150, 1.556511641 /)

   RES_R = atanRF(R)
   do i = 1, 7
      if (.NOT. precision_r4(RES_R(i), R_R(i))) call zzrc(i)
   end do
   RES_C = atanCF(C)
   do i = 1, 7
      if (.NOT. precision_x8(RES_C(i), R_C(i))) call zzrc(i+7)
   end do

   ASSOCIATE ( a => atan(1.0), b => atan( (1.0,0.75) ) )
      if (tan(a) .NE. 1.0) call zzrc(i+8)
      if (tan(b) .NE. (1.0,0.75)) call zzrc(i+9)
   END ASSOCIATE

end program main
