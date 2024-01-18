!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : logGammaIntrinsicF2.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2010-11-16
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : LOG_GAMMA in elemental function
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

   real elemental function logGammaRF(arrayR)
      real(4), intent(in) :: arrayR
      logGammaRF = log_gamma(arrayR)
      return
   end function logGammaRF

end module trig

program main

   use trig

   implicit none

   integer i
   double precision D
   real(4), dimension(7) :: N, R, RES
   external precision_r4; logical(4) precision_r4
   N = (/ 1.0, 3.0,          5.0,         11.0,        17.0,        23.0,        33.0  /)
   R = (/ 0.0, 0.6931471825, 3.178053856, 15.10441303, 30.67185974, 48.47117996, 81.55796051 /)

   RES = logGammaRF(N)
   do i = 1, 7
      if (.NOT. precision_r4(RES(i), R(i))) call zzrc(i)
   end do

   D = 3.456789
   if (log_gamma(D) .NE. log(gamma(D))) call zzrc(i)

   ASSOCIATE (a => log_gamma(13.75))
      if (log(gamma(13.75)) .NE. a) call zzrc(i+1)
   END ASSOCIATE

end program main
