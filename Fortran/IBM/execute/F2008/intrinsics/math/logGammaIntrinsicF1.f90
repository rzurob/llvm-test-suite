!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : logGammaIntrinsicF1.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2010-11-16
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : LOG_GAMMA
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
!*  This program tests the LOG_GAMMA(x) intrinsic procedure where (-1 <= x <= 1)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module trig

   contains

   subroutine logGammaRS(r, res)
      real(4), intent(in) :: r
      real(4) res
      res = log_gamma(r)
      return
   end subroutine logGammaRS

   real function logGammaRF(r)
      real(4), intent(in) :: r
      logGammaRF = log_gamma(r)
      return
   end function logGammaRF

end module trig

program main

   use trig

   implicit none

   integer i
   real(4), parameter :: init_value = log_gamma(3.0)
   real(4) RES
   real(4), dimension(7) :: N, R
   external precision_r4; logical(4) precision_r4
   N = (/ 1.0, 3.0,          5.0,         11.0,        17.0,        23.0,        33.0  /)
   R = (/ 0.0, 0.6931471825, 3.178053856, 15.10441303, 30.67185974, 48.47117996, 81.55796051 /)

   do i = 1, 7
      RES = log_gamma(N(i))
      !
      ! Check if the value computed is right
      !
      if (.NOT. precision_r4(RES, R(i))) then
         call zzrc(i+1)
      else
         !
         ! Compute the gamma and then log and then compare with the above result
         !
         if (.NOT. precision_r4(log(gamma(N(i))), RES)) then
            call zzrc(i+2)
         end if
      end if
   end do

   call logGammaRS(N(4), RES)
   if (.NOT. precision_r4(RES, R(4))) then
      call zzrc(i+2)
   end if
   if (.NOT. precision_r4(logGammaRF(N(4)), R(4))) then
      call zzrc(i+3)
   end if

   if (log_gamma(3.0) .NE. init_value) then
      call zzrc(i+4)
   end if

end program main
