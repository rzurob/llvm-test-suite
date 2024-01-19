!*  ============================================================================
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
!*  type character and zero level nesting
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module testM

   contains

   subroutine test(a, b)
      character(*), intent(in) :: a
      character(*), optional :: b
      print *,a
      print *,present(b)
   end subroutine test

end module testM

program main

   use testM
   implicit none
   character, pointer :: ch

   nullify (ch)
	call test ("Hello World")
	call test ("Hello World", ch)

end program main

