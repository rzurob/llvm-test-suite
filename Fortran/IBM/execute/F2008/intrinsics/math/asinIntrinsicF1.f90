!*  ============================================================================
!*
!*  TEST CASE NAME             : asinIntrinsicF1.f
!*
!*  DATE                       : 2010-11-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ASIN
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
!*  This program tests the ASIN(x) intrinsic procedure where (-1 <= x <= 1)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module trig

   contains

   subroutine asinRS(r, res)
      real(4), intent(in) :: r
      real(4) res
      res = asin(r)
      return
   end subroutine asinRS

   real function asinRF(r)
      real(4), intent(in) :: r
      asinRF = asin(r)
      return
   end function asinRF

   subroutine asinCS(c, res)
      complex(4), intent(in) :: c
      complex(4) res
      res = asin(c)
      return
   end subroutine asinCS

   complex function asinCF(c)
      complex(4), intent(in) :: c
      asinCF = asin(c)
      return
   end function asinCF

end module trig

program main

   use trig

   implicit none

   integer i
   complex(4), parameter :: init_value = asin((0.75,0.5))
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
   R_C = (/ (0.8959074616,0.7328577042), (0.7649595141,0.4047130644), (0.5090856552,0.2270882726), (0.0000000000E+00,0.9983407706E-01), (-0.4304306805,0.6197743416), (-0.6074215174,0.7731347680), (-0.7031000257,1.002976656) /)
   R = (/ 1.0, 0.75, 0.5, 0.0, -0.5, -0.75, -1.0 /)
   R_R = (/ 1.570796371, 0.8480620980, 0.5235987902, 0.0, -0.5235987902, -0.8480620980, -1.570796371 /)

   do i = 1, 7
      RES_C = asin(C(i))
      !
      ! Check if the value computed is right for complex number
      !
      if (.NOT. precision_x8(RES_C, R_C(i))) then
         call zzrc(i+1)
      else
         !
         ! Check if the value computed is right for real number
         !
         RES_R = asin(R(i))
         if (.NOT. precision_r4(RES_R, R_R(i))) then
            call zzrc(i+2)
         !
         ! Compute the sin and check if its equal to the real number
         !
         else if (.NOT. precision_r4(sin(RES_R), R(i))) then
            call zzrc(i+3)
         end if
      end if
   end do

   call asinRS(R(4), RES_R)
   if (.NOT. precision_r4(RES_R, R_R(4))) then
      call zzrc(i+3)  ! 11
   end if
   call asinCS(C(4), RES_C)
   if (.NOT. precision_x8(RES_C, R_C(4))) then
      call zzrc(i+4)  ! 12
   end if
   if (.NOT. precision_r4(asinRF(R(4)), R_R(4))) then
      call zzrc(i+5)  ! 13
   else if (.NOT. precision_x8(asinCF(C(4)), R_C(4))) then
      call zzrc(i+6)  ! 14
   end if

   if (asin((0.75,0.5)) .NE. init_value) then
      call zzrc(i+7)
   end if

end program main
