!*  ============================================================================
!*
!*  TEST CASE NAME             : enhDummyArg02.f
!*
!*  DATE                       : 2011-08-08
!*
!*  PRIMARY FUNCTIONS TESTED   : Enhancement to determining dummy argument presence
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 386700
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  type character and one level nesting
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module testM

   contains

   subroutine test(a, b)
      character(*), intent(in) :: a
      character(*), optional :: b
      call test_1(a, b)
   end subroutine test

   subroutine test_1(a, b)
      character(*), intent(in) :: a
      character(*), optional :: b
      print *,a
      print *,present(b)
   end subroutine test_1

end module testM

program main

   use testM
   implicit none
   character, allocatable :: ch

	call test ("Hello World")
	call test ("Hello World", ch)

end program main

