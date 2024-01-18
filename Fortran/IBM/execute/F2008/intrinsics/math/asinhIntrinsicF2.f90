!*  ============================================================================
!*
!*  TEST CASE NAME             : asinhIntrinsicF2.f
!*
!*  DATE                       : 2010-11-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ASINH in elemental function
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
!*  This program tests the ASINH(x) intrinsic procedure
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module trig

   contains

   real elemental function asinhRF(arrayR)
      real(4), intent(in) :: arrayR
      asinhRF = asinh(arrayR)
      return
   end function asinhRF

   complex elemental function asinhCF(arrayC)
      complex(4), intent(in) :: arrayC
      asinhCF = asinh(arrayC)
      return
   end function asinhCF

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
   R_C = (/ (0.9261330366,0.3494390547), (0.7107204795,0.2397496551), (0.4884828031,0.1792594641), (0.0000000000E+00,0.1001674235), (-0.5555174947,0.5445070863), (-0.7949108481,0.5529005527), (-1.027543545,0.6077864766) /)
   R = (/ 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0 /)
   R_R = (/ 2.998223066, 3.689503908, 4.094622135, 4.382183075, 4.605270386, 4.787561417, 4.941693306 /)

   RES_R = asinhRF(R)
   do i = 1, 7
      if (.NOT. precision_r4(RES_R(i), R_R(i))) call zzrc(i)
   end do
   RES_C = asinhCF(C)
   do i = 1, 7
      if (.NOT. precision_x8(RES_C(i), R_C(i))) call zzrc(i+7)
   end do

   D = 3.456789
   if (sinh(asinh(D)) .NE. D) call zzrc(i+7)

   ASSOCIATE ( a => asinh(1.0), b => asinh( (1.0,0.75) ) )
      if (sinh(a) .NE. 1.0) call zzrc(i+8)
      if (sinh(b) .NE. (1.0,0.75)) call zzrc(i+9)
   END ASSOCIATE

end program main
