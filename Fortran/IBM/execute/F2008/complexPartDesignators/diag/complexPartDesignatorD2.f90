!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : complexPartDesignatorD2.f
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
!*     Test with different types, also check the type of complex-part-designator
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module m

   contains

   subroutine m_l(l)
      logical, intent(in) :: l
      print *,l
   end subroutine

   subroutine m_ch(ch)
      character, intent(in) :: ch
   end subroutine

   subroutine m_i(i)
      integer, intent(in) :: i
   end subroutine

   subroutine m_r(r)
      real, intent(in) :: r
   end subroutine

   subroutine m_c(c)
      complex, intent(in) :: c
   end subroutine

end module m

program main

   use m
   implicit none

   logical l
   character ch
   integer i
   real r
   complex c

   c%RE = 1.5                  ! No error
   c%IM = 0.5                  ! No error
   c%RE = 'C'                  ! Type error
   c%IM = 'C'                  ! Type error
   c%RE = "STRING"             ! Type error
   c%IM = "STRING"             ! Type error

   l%RE  = 1.5                 ! Type error
   l%IM  = 0.5                 ! Type error
   ch%RE = 1.5                 ! Type error
   ch%IM = 0.5                 ! Type error
   i%RE  = 1.5                 ! Type error
   i%IM  = 0.5                 ! Type error
   r%RE  = 1.5                 ! Type error
   r%IM  = 0.5                 ! Type error

   c%RE = l%RE                 ! Type error
   c%IM = l%IM                 ! Type error
   c%RE = ch%RE                ! Type error
   c%IM = ch%IM                ! Type error
   c%RE = i%RE                 ! Type error
   c%IM = i%IM                 ! Type error
   c%RE = r%RE                 ! Type error
   c%IM = r%IM                 ! Type error

   call m_l(c%RE)              ! Argument type error
   call m_l(c%IM)              ! Argument type error
   call m_ch(c%RE)             ! Argument type error
   call m_ch(c%IM)             ! Argument type error
   call m_i(c%RE)              ! Argument type error
   call m_i(c%IM)              ! Argument type error
   call m_r(c%RE)              ! No error
   call m_r(c%IM)              ! No error
   call m_c(c%RE)              ! Argument type error
   call m_c(c%IM)              ! Argument type error

end program main
