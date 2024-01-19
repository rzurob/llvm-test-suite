!*  ============================================================================
!*
!*  DATE                       : 2010-11-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ACOS in elemental function
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
!*  This program tests the ACOS(x) intrinsic procedure where (-1 <= x <= 1)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module trig

   contains

   real elemental function acosRF(arrayR)
      real(4), intent(in) :: arrayR
      acosRF = acos(arrayR)
      return
   end function acosRF

   complex elemental function acosCF(arrayC)
      complex(4), intent(in) :: arrayC
      acosCF = acos(arrayC)
      return
   end function acosCF

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
   R_C = (/ (0.6748888493,-0.7328577042), (0.8058367968,-0.4047130644), (1.061710715,-0.2270882726), (1.570796371,-0.9983407706E-01), (2.001226902,-0.6197743416), (2.178217888,-0.7731347680), (2.273896456,-1.002976656) /)
   R = (/ 1.0, 0.75, 0.5, 0.0, -0.5, -0.75, -1.0 /)
   R_R = (/ 0.0, 0.7227342725, 1.047197580, 1.570796371, 2.094395161, 2.418858290, 3.141592741 /)

   RES_R = acosRF(R)
   do i = 1, 7
      if (.NOT. precision_r4(RES_R(i), R_R(i))) call zzrc(i)
   end do
   RES_C = acosCF(C)
   do i = 1, 7
      if (.NOT. precision_x8(RES_C(i), R_C(i))) call zzrc(i+7)
   end do
   print *, "Debug: i=", i
   ASSOCIATE ( a => acos(1.0), b => acos( (1.0,0.75) ) )
      if (cos(a) .NE. 1.0) call zzrc(i+8)
      if (.NOT. precision_x8(cos(b), (1.0,0.75))) call zzrc(i+9)
   END ASSOCIATE

end program main
