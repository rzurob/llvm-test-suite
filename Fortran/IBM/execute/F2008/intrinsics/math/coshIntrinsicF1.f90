!*  ============================================================================
!*
!*  TEST CASE NAME             : coshIntrinsicF1.f
!*
!*  DATE                       : 2010-11-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : COSH
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

   subroutine coshRS(r, res)
      real(4), intent(in) :: r
      real(4) res
      res = cosh(r)
      return
   end subroutine coshRS

   real function coshRF(r)
      real(4), intent(in) :: r
      coshRF = cosh(r)
      return
   end function coshRF

   subroutine coshCS(c, res)
      complex(4), intent(in) :: c
      complex(4) res
      res = cosh(c)
      return
   end subroutine coshCS

   complex function coshCF(c)
      complex(4), intent(in) :: c
      coshCF = cosh(c)
      return
   end function coshCF

end module trig

program main

   use trig

   implicit none

   integer i
   complex(4), parameter :: init_value = cosh((0.75,0.5))
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
   R_C = (/ (1.354180694,0.5634214878), (1.236858130,0.2430112213), (1.105148554,0.1035256535), (0.9950041771,0.0000000000E+00), (0.9306698442,-0.2942325473), (0.9902284145,-0.5297510028), (0.9591943622,-0.9205666780) /)
   R = (/ 1.0, 0.75, 0.5, 0.0, -0.5, -0.75, -1.0 /)
   R_R = (/ 1.543080688, 1.294683337, 1.127625942, 1.0, 1.127625942, 1.294683337, 1.543080688 /)

   do i = 1, 7
      RES_C = cosh(C(i))
      !
      ! Check if the value computed is right for complex number
      !
      if (.NOT. precision_x8(RES_C, R_C(i))) then
         call zzrc(i+1)
      else
         !
         ! Check if the value computed is right for real number
         !
         RES_R = cosh(R(i))
         if (.NOT. precision_r4(RES_R, R_R(i))) then
            call zzrc(i+2)
         !
         ! Compute the acosh and check if its equal to the real number
         !
         else if (.NOT. precision_r4(acosh(RES_R), abs(R(i)))) then
            call zzrc(i+3)
         end if
      end if
   end do

   call coshRS(R(4), RES_R)
   if (.NOT. precision_r4(RES_R, R_R(4))) then
      call zzrc(i+3)  ! 11
   end if
   call coshCS(C(4), RES_C)
   if (.NOT. precision_x8(RES_C, R_C(4))) then
      call zzrc(i+4)  ! 12
   end if
   if (.NOT. precision_r4(coshRF(R(4)), R_R(4))) then
      call zzrc(i+5)  ! 13
   else if (.NOT. precision_x8(coshCF(C(4)), R_C(4))) then
      call zzrc(i+6)  ! 14
   end if

   if (cosh((0.75,0.5)) .NE. init_value) then
      call zzrc(i+7)
   end if

end program main
