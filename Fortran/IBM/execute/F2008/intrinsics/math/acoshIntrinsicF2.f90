!*  ============================================================================
!*
!*  DATE                       : 2010-11-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ACOSH in elemental function
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
!*  This program tests the ACOSH(x) intrinsic procedure
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module trig

   contains

   real elemental function acoshRF(arrayR)
      real(4), intent(in) :: arrayR
      acoshRF = acosh(arrayR)
      return
   end function acoshRF

   complex elemental function acoshCF(arrayC)
      complex(4), intent(in) :: arrayC
      acoshCF = acosh(arrayC)
      return
   end function acoshCF

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
   R_C = (/ (0.7328577042,0.6748888493), (0.4047130644,0.8058367968), (0.2270882726,1.061710715), (0.9983407706E-01,1.570796371), (0.6197743416,2.001226902), (0.7731347680,2.178217888), (1.002976656,2.273896456) /)
   R = (/ 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0 /)
   R_R = (/ 2.993222952, 3.688253880, 4.094066620, 4.381870270, 4.605070114, 4.787422180, 4.941591263 /)

   RES_R = acoshRF(R)
   do i = 1, 7
      if (.NOT. precision_r4(RES_R(i), R_R(i))) call zzrc(i)
   end do
   RES_C = acoshCF(C)
   do i = 1, 7
      if (.NOT. precision_x8(RES_C(i), R_C(i))) call zzrc(i+7)
   end do

   D = 3.123456789
   if (cosh(acosh(D)) .NE. D) call zzrc(i+7)

   ASSOCIATE ( a => acosh(1.0), b => acosh( (1.0,0.75) ) )
      if (cosh(a) .NE. 1.0) call zzrc(i+8)
      if (.NOT. precision_x8(cosh(b), (1.0,0.75))) call zzrc(i+9)
   END ASSOCIATE

end program main
