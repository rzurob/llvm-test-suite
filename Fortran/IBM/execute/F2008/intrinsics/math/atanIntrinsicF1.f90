!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : atanIntrinsicF1.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2010-11-16
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ATAN
!*
!*  REFERENCE                  : Feature Number 376003
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  This program tests the ATAN(x) intrinsic procedure where (-1 <= x <= 1)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module trig

   contains

   subroutine atanRS(r, res)
      real(4), intent(in) :: r
      real(4) res
      res = atan(r)
      return
   end subroutine atanRS

   real function atanRF(r)
      real(4), intent(in) :: r
      atanRF = atan(r)
      return
   end function atanRF

   subroutine atanCS(c, res)
      complex(4), intent(in) :: c
      complex(4) res
      res = atan(c)
      return
   end subroutine atanCS

   complex function atanCF(c)
      complex(4), intent(in) :: c
      atanCF = atan(c)
      return
   end function atanCF

end module trig

program main

   use trig

   implicit none

   integer i
   complex(4), parameter :: init_value = atan((0.75,0.5))
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
   R_C = (/ (0.8475756645,0.2388778627), (0.6715728045,0.1902181208), (0.4766952097,0.1603155881), (0.0000000000E+00,0.1003353521), (-0.5994701385,0.4811956584), (-0.8028910160,0.4165106714), (-0.9778028131,0.3795693815) /)
   R = (/ 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0 /)
   R_R = (/ 1.471127629, 1.520837903, 1.537475348, 1.545801520, 1.550799012, 1.554131150, 1.556511641 /)

   do i = 1, 7
      RES_C = atan(C(i))
      !
      ! Check if the value computed is right for complex number
      !
      if (.NOT. precision_x8(RES_C, R_C(i))) then
         call zzrc(i+1)
      else
         !
         ! Check if the value computed is right for real number
         !
         RES_R = atan(R(i))
         if (.NOT. precision_r4(RES_R, R_R(i))) then
            call zzrc(i+2)
         !
         ! Compute the tan and check if its equal to the real number
         !
         else if (.NOT. precision_r4(tan(RES_R), R(i))) then
            call zzrc(i+3)
         end if
      end if
   end do

   call atanRS(R(4), RES_R)
   if (.NOT. precision_r4(RES_R, R_R(4))) then
      call zzrc(i+3)  ! 11
   end if
   call atanCS(C(4), RES_C)
   if (.NOT. precision_x8(RES_C, R_C(4))) then
      call zzrc(i+4)  ! 12
   end if
   if (.NOT. precision_r4(atanRF(R(4)), R_R(4))) then
      call zzrc(i+5)  ! 13
   else if (.NOT. precision_x8(atanCF(C(4)), R_C(4))) then
      call zzrc(i+6)  ! 14
   end if

   if (atan((0.75,0.5)) .NE. init_value) then
      call zzrc(i+7)
   end if

end program main
