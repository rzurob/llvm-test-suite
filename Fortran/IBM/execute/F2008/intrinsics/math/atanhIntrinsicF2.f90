!*  ============================================================================
!*
!*  DATE                       : 2010-11-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ATANH in elemental function
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
!*  This program tests the ATANH(x) intrinsic procedure
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module trig

   contains

   real elemental function atanhRF(arrayR)
      real(4), intent(in) :: arrayR
      atanhRF = atanh(arrayR)
      return
   end function atanhRF

   complex elemental function atanhCF(arrayC)
      complex(4), intent(in) :: arrayC
      atanhCF = atanh(arrayC)
      return
   end function atanhCF

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
   R_C = (/ (0.7083033323,0.9078875184), (0.7571966052,0.5229181647), (0.5166065693,0.2565289438), (0.0000000000E+00,0.9966865182E-01), (-0.3634116352,0.6282822490), (-0.4652383626,0.8041393757), (-0.4453545511,0.9968250990) /)
   R = (/ 0.9, 0.75, 0.5, 0.0, -0.5, -0.75, -0.9 /)
   R_R = (/ 1.472219348, 0.9729550481, 0.5493061543, 0.0, -0.5493061543, -0.9729550481, -1.472219348 /)

   RES_R = atanhRF(R)
   do i = 1, 7
      if (.NOT. precision_r4(RES_R(i), R_R(i))) call zzrc(i)
   end do
   RES_C = atanhCF(C)
   do i = 1, 7
      if (.NOT. precision_x8(RES_C(i), R_C(i))) call zzrc(i+7)
   end do

   D = 0.987654321
   if (tanh(atanh(D)) .NE. D) call zzrc(i+7)

   ASSOCIATE ( a => atanh(1.0), b => atanh( (1.0,0.75) ) )
      if (tanh(a) .NE. 1.0) call zzrc(i+8)
      if (.NOT. precision_x8(tanh(b), (1.0,0.75))) call zzrc(i+9)
   END ASSOCIATE

end program main
