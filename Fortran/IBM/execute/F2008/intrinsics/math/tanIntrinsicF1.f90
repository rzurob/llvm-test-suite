!*  ============================================================================
!*
!*  TEST CASE NAME             : tanIntrinsicF1.f
!*
!*  DATE                       : 2010-11-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : TAN
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
!*  This program tests the TAN(x) intrinsic procedure where (-1 <= x <= 1)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module trig

   contains

   subroutine tanRS(r, res)
      real(4), intent(in) :: r
      real(4) res
      res = tan(r)
      return
   end subroutine tanRS

   real function tanRF(r)
      real(4), intent(in) :: r
      tanRF = tan(r)
      return
   end function tanRF

   subroutine tanCS(c, res)
      complex(4), intent(in) :: c
      complex(4) res
      res = tan(c)
      return
   end subroutine tanCS

   complex function tanCF(c)
      complex(4), intent(in) :: c
      tanCF = tan(c)
      return
   end function tanCF

end module trig

program main

   use trig

   implicit none

   integer i
   complex(4), parameter :: init_value = tan((0.75,0.5))
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
   R_C = (/ (0.8068774343,1.042830706), (0.7940559387,0.5068081617), (0.5189861655,0.2533358634), (0.0000000000E+00,0.9966799617E-01), (-0.3579268456,0.6420623064), (-0.4489912689,0.8571619391), (-0.3378622234,1.093206048) /)
   R = (/ 1.0, 0.75, 0.5, 0.0, -0.5, -0.75, -1.0 /)
   R_R = (/ 1.557407737, 0.9315964580, 0.5463024974, 0.0, -0.5463024974, -0.9315964580, -1.557407737 /)

   do i = 1, 7
      RES_C = tan(C(i))
      !
      ! Check if the value computed is right for complex number
      !
      if (.NOT. precision_x8(RES_C, R_C(i))) then
         call zzrc(i+1)
      else
         !
         ! Check if the value computed is right for real number
         !
         RES_R = tan(R(i))
         if (.NOT. precision_r4(RES_R, R_R(i))) then
            call zzrc(i+2)
         !
         ! Compute the atan and check if its equal to the real number
         !
         else if (.NOT. precision_r4(atan(RES_R), R(i))) then
            call zzrc(i+3)
         end if
      end if
   end do

   call tanRS(R(4), RES_R)
   if (.NOT. precision_r4(RES_R, R_R(4))) then
      call zzrc(i+3)  ! 11
   end if
   call tanCS(C(4), RES_C)
   if (.NOT. precision_x8(RES_C, R_C(4))) then
      call zzrc(i+4)  ! 12
   end if
   if (.NOT. precision_r4(tanRF(R(4)), R_R(4))) then
      call zzrc(i+5)  ! 13
   else if (.NOT. precision_x8(tanCF(C(4)), R_C(4))) then
      call zzrc(i+6)  ! 14
   end if

   if (tan((0.75,0.5)) .NE. init_value) then
      call zzrc(i+7)
   end if

end program main
