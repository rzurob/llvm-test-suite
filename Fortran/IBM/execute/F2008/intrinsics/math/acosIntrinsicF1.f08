!*  ============================================================================
!*
!*  DATE                       : 2010-11-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ACOS
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

   subroutine acosRS(r, res)
      real(4), intent(in) :: r
      real(4) res
      res = acos(r)
      return
   end subroutine acosRS

   real function acosRF(r)
      real(4), intent(in) :: r
      acosRF = acos(r)
      return
   end function acosRF

   subroutine acosCS(c, res)
      complex(4), intent(in) :: c
      complex(4) res
      res = acos(c)
      return
   end subroutine acosCS

   complex function acosCF(c)
      complex(4), intent(in) :: c
      acosCF = acos(c)
      return
   end function acosCF

end module trig

program main

   use trig

   implicit none

   integer i
   complex(4), parameter :: init_value = acos((0.75,0.5))
   real(4) RES_R
   complex(4) RES_C
   real(4), dimension(7) :: R, R_R
   complex(4), dimension(7) :: C, R_C

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

    do i = 1, 7
      RES_C = acos(C(i))
      !
      ! Check if the value computed is right for complex number
      !
      if (.NOT. precision_x8(RES_C, R_C(i))) then
         call zzrc(i+1)
      else
         !
         ! Check if the value computed is right for real number
         !
         RES_R = acos(R(i))
         if (.NOT. precision_r4(RES_R, R_R(i))) then
            call zzrc(i+2)
         else if (abs(RES_R) .LE. 1) then
            !
            ! Compute the cos and check if its equal to the real number
            !
            if (.NOT. precision_r4(cos(RES_R), R(i))) then
               call zzrc(i+3)
            end if
         end if
      end if
   end do

   call acosRS(R(4), RES_R)
   if (.NOT. precision_r4(RES_R, R_R(4))) then
      call zzrc(i+3)  ! 11
   end if
   call acosCS(C(4), RES_C)
   if (.NOT. precision_x8(RES_C, R_C(4))) then
      call zzrc(i+4)  ! 12
   end if
   if (.NOT. precision_r4(acosRF(R(4)), R_R(4))) then
      call zzrc(i+5)  ! 13
   else if (.NOT. precision_x8(acosCF(C(4)), R_C(4))) then
      call zzrc(i+6)  ! 14
   end if

   if (acos((0.75,0.5)) .NE. init_value) then
      call zzrc(i+7)
   end if

end program main
