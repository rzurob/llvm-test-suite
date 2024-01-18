!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : intrinsic01D.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2010-11-10
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Math Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ACOSH, ASINH, ATANH and LOG_GAMMA
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
!*  This program tests the new  Trignometry intrinsic functions in F2008.
!*  Diagnostic test checking with different arguments:
!*     integer
!*     character
!*     string
!*     logical
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

   implicit none

   real result
   real(4), dimension(1) :: R_4
   real(8), dimension(1) :: R_8
   complex(4), dimension(1) :: C_4

   R_4 = (/ 0.4567 /)
   R_8 = (/ 0.8910 /)
   C_4 = (/ (1.0, 0.4567) /)

   result = acosh(1)                  ! Error
   result = acosh(0)                  ! Error
   result = acosh(-1)                 ! Error
   result = acosh('C')                ! Error
   result = acosh("testing")          ! Error
   result = acosh(.TRUE.)             ! Error
   result = acosh(.FALSE.)            ! Error
   result = acosh(R_4(1), R_4(1))     ! Error
   result = acosh(R_4(1))             ! No error
   result = acosh(C_4(1))             ! No error

   result = asinh(1)                  ! Error
   result = asinh(0)                  ! Error
   result = asinh(-1)                 ! Error
   result = asinh('C')                ! Error
   result = asinh("testing")          ! Error
   result = asinh(.TRUE.)             ! Error
   result = asinh(.FALSE.)            ! Error
   result = asinh(R_4(1), R_4(1))     ! Error
   result = asinh(R_4(1))             ! No error
   result = asinh(C_4(1))             ! No error

   result = atanh(1)                  ! Error
   result = atanh(0)                  ! Error
   result = atanh(-1)                 ! Error
   result = atanh('C')                ! Error
   result = atanh("testing")          ! Error
   result = atanh(.TRUE.)             ! Error
   result = atanh(.FALSE.)            ! Error
   result = atanh(R_4(1), R_4(1))     ! Error
   result = atanh(R_4(1))             ! No error
   result = atanh(C_4(1))             ! No error

   result = atan(0, 0)                ! Error
   result = atan(C_4(1), C_4(1))      ! Error
   result = atan(R_4(1), R_8(1))      ! Error
   result = atan(R_4(1), R_4(1))      ! No Error
   result = atan(C_4(1))              ! No Error

   result = log_gamma(1)              ! Error
   result = log_gamma(0)              ! Error
   result = log_gamma(-1)             ! Error
   result = log_gamma('C')            ! Error
   result = log_gamma("testing")      ! Error
   result = log_gamma(.TRUE.)         ! Error
   result = log_gamma(.FALSE.)        ! Error
   result = log_gamma(R_4(1), R_4(1)) ! Error
   result = log_gamma(R_4(1))         ! No error
   result = log_gamma(C_4(1))         ! Error

end program main
