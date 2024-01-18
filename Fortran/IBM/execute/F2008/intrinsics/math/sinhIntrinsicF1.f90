!*  ============================================================================
!*
!*  TEST CASE NAME             : sinhIntrinsicF1.f
!*
!*  DATE                       : 2010-11-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : SINH
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
!*  This program tests the SINH(x) intrinsic procedure where (-1 <= x <= 1)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module trig

   contains

   subroutine sinhRS(r, res)
      real(4), intent(in) :: r
      real(4) res
      res = sinh(r)
      return
   end subroutine sinhRS

   real function sinhRF(r)
      real(4), intent(in) :: r
      sinhRF = sinh(r)
      return
   end function sinhRF

   subroutine sinhCS(c, res)
      complex(4), intent(in) :: c
      complex(4) res
      res = sinh(c)
      return
   end subroutine sinhCS

   complex function sinhCF(c)
      complex(4), intent(in) :: c
      sinhCF = sinh(c)
      return
   end function sinhCF

end module trig

program main

   use trig

   implicit none

   integer i
   complex(4), parameter :: init_value = sinh((0.75,0.5))
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
   R_C = (/ (1.031336069,0.7397922873), (0.7855891585,0.3826050758), (0.5107080936,0.2240246981), (0.0000000000E+00,0.9983342141E-01), (-0.4300785065,0.6367055178), (-0.6289425492,0.8340578675), (-0.7305167913,1.208736539) /)
   R = (/ 1.0, 0.75, 0.5, 0.0, -0.5, -0.75, -1.0 /)
   R_R = (/ 1.175201178, 0.8223167062, 0.5210952759, 0.0, -0.5210952759, -0.8223167062, -1.175201178 /)

   do i = 1, 7
      RES_C = sinh(C(i))
      !
      ! Check if the value computed is right for complex number
      !
      if (.NOT. precision_x8(RES_C, R_C(i))) then
         call zzrc(i+1)
      else
         !
         ! Check if the value computed is right for real number
         !
         RES_R = sinh(R(i))
         if (.NOT. precision_r4(RES_R, R_R(i))) then
            call zzrc(i+2)
         !
         ! Compute the asinh and check if its equal to the real number
         !
         else if (.NOT. precision_r4(asinh(RES_R), R(i))) then
            call zzrc(i+3)
         end if
      end if
   end do

   call sinhRS(R(4), RES_R)
   if (.NOT. precision_r4(RES_R, R_R(4))) then
      call zzrc(i+3)  ! 11
   end if
   call sinhCS(C(4), RES_C)
   if (.NOT. precision_x8(RES_C, R_C(4))) then
      call zzrc(i+4)  ! 12
   end if
   if (.NOT. precision_r4(sinhRF(R(4)), R_R(4))) then
      call zzrc(i+5)  ! 13
   else if (.NOT. precision_x8(sinhCF(C(4)), R_C(4))) then
      call zzrc(i+6)  ! 14
   end if

   if (sinh((0.75,0.5)) .NE. init_value) then
      call zzrc(i+7)
   end if

end program main
