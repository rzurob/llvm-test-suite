!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : complexPartDesignatorD1.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2011-01-17
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Complex Part Designator
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 383634
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
!*  This program tests the complex part designator:
!*     Test with different syntaxes
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

   implicit none

   logical l
   character ch
   integer i
   real r
   complex c

   c%REAL = 1.5                ! Syntax error
   c%IMAG = 0.5                ! Syntax error
   c%RE => 1.5                 ! Syntax error
   c%IM => 0.5                 ! Syntax error
   r = (1.5, 0.5)%RE           ! Syntax error
   r = (1.5, 0.5)%IM           ! Syntax error
   r = (1.5)%RE                ! Syntax error
   r = (1.5)%IM                ! Syntax error

   c = (1.75, 0.75)
   l = c%RE                    ! Type conversion error
   l = c%IM                    ! Type conversion error
   ch = c%RE                   ! Type conversion error
   ch = c%IM                   ! Type conversion error
   i = c%RE                    ! No error
   i = c%IM                    ! No error
   r = c%RE                    ! No error
   r = c%IM                    ! No error
   c = c%RE                    ! No error
   c = c%IM                    ! No error

   r = real(C%IM)              ! No error
   r = real(C%RE)              ! No error
   r = imag(C%RE)              ! Wrong arguments type
   r = aimag(C%RE)             ! Wrong arguments type

end program main
