!*  ============================================================================
!*
!*  TEST CASE NAME             : tanhIntrinsicF1.f
!*
!*  DATE                       : 2010-11-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : TANH
!*
!*  REFERENCE                  : Feature Number 376003
!*
!*  DRIVER StanhZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  This program tests the TANH(x) intrinsic procedure where (-1 <= x <= 1)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module trig

   contains

   subroutine tanhRS(r, res)
      real(4), intent(in) :: r
      real(4) res
      res = tanh(r)
      return
   end subroutine tanhRS

   real function tanhRF(r)
      real(4), intent(in) :: r
      tanhRF = tanh(r)
      return
   end function tanhRF

   subroutine tanhCS(c, res)
      complex(4), intent(in) :: c
      complex(4) res
      res = tanh(c)
      return
   end subroutine tanhCS

   complex function tanhCF(c)
      complex(4), intent(in) :: c
      tanhCF = tanh(c)
      return
   end function tanhCF

end module trig

program main

   use trig

   implicit none

   integer i
   complex(4), parameter :: init_value = tanh((0.75,0.5))
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
   R_C = (/ (0.8429661989,0.1955773085), (0.6700598598,0.1776865274), (0.4769211113,0.1580340713), (0.0000000000E+00,0.1003346741), (-0.6167616248,0.4891468287), (-0.8441559672,0.3906829953), (-1.025987864,0.2754878104) /)
   R = (/ 1.0, 0.75, 0.5, 0.0, -0.5, -0.75, -1.0 /)
   R_R = (/ 0.7615941763, 0.6351489425, 0.4621171653, 0.0, -0.4621171653, -0.6351489425, -0.7615941763 /)

   do i = 1, 7
      RES_C = tanh(C(i))
      !
      ! Check if the value computed is right for complex number
      !
      if (.NOT. precision_x8(RES_C, R_C(i))) then
         call zzrc(i+1)
      else
         !
         ! Check if the value computed is right for real number
         !
         RES_R = tanh(R(i))
         if (.NOT. precision_r4(RES_R, R_R(i))) then
            call zzrc(i+2)
         !
         ! Compute the atanh and check if its equal to the real number
         !
         else if (.NOT. precision_r4(atanh(RES_R), R(i))) then
            call zzrc(i+3)
         end if
      end if
   end do

   call tanhRS(R(4), RES_R)
   if (.NOT. precision_r4(RES_R, R_R(4))) then
      call zzrc(i+3)  ! 11
   end if
   call tanhCS(C(4), RES_C)
   if (.NOT. precision_x8(RES_C, R_C(4))) then
      call zzrc(i+4)  ! 12
   end if
   if (.NOT. precision_r4(tanhRF(R(4)), R_R(4))) then
      call zzrc(i+5)  ! 13
   else if (.NOT. precision_x8(tanhCF(C(4)), R_C(4))) then
      call zzrc(i+6)  ! 14
   end if

   if (tanh((0.75,0.5)) .NE. init_value) then
      call zzrc(i+7)
   end if

end program main
