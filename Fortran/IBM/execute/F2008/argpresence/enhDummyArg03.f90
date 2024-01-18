!*  ============================================================================
!*
!*  TEST CASE NAME             : enhDummyArg03.f
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
!*  type character and two level nesting
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module testM

   contains

   subroutine test(a, b)
      character(*), intent(in) :: a
      character(*), optional, intent(inout) :: b
      call test_1(a, b)
   end subroutine test

   subroutine test_1(a, b)
      character(*), intent(in) :: a
      character(*), optional, intent(inout) :: b
      call test_2(a, b)
   end subroutine test_1

   subroutine test_2(a, b)
      character(*), intent(in) :: a
      character(*), optional, intent(inout) :: b
      print *,a
      print *,present(b)
   end subroutine test_2

end module testM

program main

   use testM
   implicit none
   character, allocatable :: ch

	call test ("Hello World Two Level")
	call test ("Hello World Two Level", ch)

end program main

