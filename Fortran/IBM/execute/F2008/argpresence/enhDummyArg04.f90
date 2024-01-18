!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : enhDummyArg04.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2011-08-08
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Enhancement to determining dummy argument presence
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 386700
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
!*  type character and three level nesting
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module testM

   contains

   subroutine test(a, b, c)
      character(*), intent(in) :: a
      character(*), optional :: b, c
      call test_1(a, b, c)
   end subroutine test

   subroutine test_1(a, b, c)
      character(*), intent(in) :: a
      character(*), optional :: b, c
      call test_2(a, b, c)
   end subroutine test_1

   subroutine test_2(a, b, c)
      character(*), intent(in) :: a
      character(*), optional :: b, c
      call test_3(a, b, c)
   end subroutine test_2

   subroutine test_3(a, b, c)
      character(*), intent(in) :: a
      character(*), optional :: b, c
      print *,a
      print *,present(b)
      print *,present(c)
   end subroutine test_3

end module testM

program main

   use testM
   implicit none
   character, pointer :: b
   character, allocatable :: c

   nullify (b)
   allocate (c)
	call test ("Hello World Three Level")
	call test ("Hello World Three Level", b)
	call test ("Hello World Three Level", b, c)

end program main

