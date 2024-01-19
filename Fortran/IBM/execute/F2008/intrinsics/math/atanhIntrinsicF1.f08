!*  ============================================================================
!*
!*  DATE                       : 2010-11-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ATANH
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

   subroutine atanhRS(r, res)
      real(4), intent(in) :: r
      real(4) res
      res = atanh(r)
      return
   end subroutine atanhRS

   real function atanhRF(r)
      real(4), intent(in) :: r
      atanhRF = atanh(r)
      return
   end function atanhRF

   subroutine atanhCS(c, res)
      complex(4), intent(in) :: c
      complex(4) res
      res = atanh(c)
      return
   end subroutine atanhCS

   complex function atanhCF(c)
      complex(4), intent(in) :: c
      atanhCF = atanh(c)
      return
   end function atanhCF

end module trig

program main

   use trig

   implicit none

   integer i
   complex(4), parameter :: init_value = atanh((0.75,0.5))
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
   R_C = (/ (0.7083033323,0.9078875184), (0.7571966052,0.5229181647), (0.5166065693,0.2565289438), (0.0000000000E+00,0.9966865182E-01), (-0.3634116352,0.6282822490), (-0.4652383626,0.8041393757), (-0.4453545511,0.9968250990) /)
   R = (/ 0.9, 0.75, 0.5, 0.0, -0.5, -0.75, -0.9 /)
   R_R = (/ 1.472219348, 0.9729550481, 0.5493061543, 0.0, -0.5493061543, -0.9729550481, -1.472219348 /)

   do i = 1, 7
      RES_C = atanh(C(i))
      !
      ! Check if the value computed is right for complex number
      !
      if (.NOT. precision_x8(RES_C, R_C(i))) then
         call zzrc(i+1)
      else
         !
         ! Check if the value computed is right for real number
         !
         RES_R = atanh(R(i))
         if (.NOT. precision_r4(RES_R, R_R(i))) then
            call zzrc(i+2)
         !
         ! Compute the tanh and check if its equal to the real number
         !
         else if (.NOT. precision_r4(tanh(RES_R), R(i))) then
            call zzrc(i+3)
         end if
      end if
   end do

   call atanhRS(R(4), RES_R)
   if (.NOT. precision_r4(RES_R, R_R(4))) then
      call zzrc(i+3)  ! 11
   end if
   call atanhCS(C(4), RES_C)
   if (.NOT. precision_x8(RES_C, R_C(4))) then
      call zzrc(i+4)  ! 12
   end if
   if (.NOT. precision_r4(atanhRF(R(4)), R_R(4))) then
      call zzrc(i+5)  ! 13
   else if (.NOT. precision_x8(atanhCF(C(4)), R_C(4))) then
      call zzrc(i+6)  ! 14
   end if

   if (atanh((0.75,0.5)) .NE. init_value) then
      call zzrc(i+7)
   end if

end program main
