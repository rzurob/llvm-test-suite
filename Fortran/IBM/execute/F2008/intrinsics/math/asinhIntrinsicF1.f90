!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : asinhIntrinsicF1.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2010-11-16
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ASINH
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
!*  This program tests the ASINH(x) intrinsic procedure
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module trig

   contains

   subroutine asinhRS(r, res)
      real(4), intent(in) :: r
      real(4) res
      res = asinh(r)
      return
   end subroutine asinhRS

   real function asinhRF(r)
      real(4), intent(in) :: r
      asinhRF = asinh(r)
      return
   end function asinhRF

   subroutine asinhCS(c, res)
      complex(4), intent(in) :: c
      complex(4) res
      res = asinh(c)
      return
   end subroutine asinhCS

   complex function asinhCF(c)
      complex(4), intent(in) :: c
      asinhCF = asinh(c)
      return
   end function asinhCF

end module trig

program main

   use trig

   implicit none

   integer i
   complex(4), parameter :: init_value = asinh((0.75,0.5))
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
   R_C = (/ (0.9261330366,0.3494390547), (0.7107204795,0.2397496551), (0.4884828031,0.1792594641), (0.0000000000E+00,0.1001674235), (-0.5555174947,0.5445070863), (-0.7949108481,0.5529005527), (-1.027543545,0.6077864766) /)
   R = (/ 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0 /)
   R_R = (/ 2.998223066, 3.689503908, 4.094622135, 4.382183075, 4.605270386, 4.787561417, 4.941693306 /)

    do i = 1, 7
      RES_C = asinh(C(i))
      !
      ! Check if the value computed is right for complex number
      !
      if (.NOT. precision_x8(RES_C, R_C(i))) then
         call zzrc(i+1)
      else
         !
         ! Check if the value computed is right for real number
         !
         RES_R = asinh(R(i))
         if (.NOT. precision_r4(RES_R, R_R(i))) then
            call zzrc(i+2)
         !
         ! Compute the sinh and check if its equal to the real number
         !
         else if (.NOT. precision_r4(sinh(RES_R), R(i))) then
            call zzrc(i+3)
         end if
      end if
   end do

   call asinhRS(R(4), RES_R)
   if (.NOT. precision_r4(RES_R, R_R(4))) then
      call zzrc(i+3)  ! 11
   end if
   call asinhCS(C(4), RES_C)
   if (.NOT. precision_x8(RES_C, R_C(4))) then
      call zzrc(i+4)  ! 12
   end if
   if (.NOT. precision_r4(asinhRF(R(4)), R_R(4))) then
      call zzrc(i+5)  ! 13
   else if (.NOT. precision_x8(asinhCF(C(4)), R_C(4))) then
      call zzrc(i+6)  ! 14
   end if

   if (asinh((0.75,0.5)) .NE. init_value) then
      call zzrc(i+7)
   end if

end program main
