!*  ============================================================================
!*
!*  DATE                       : 2011-08-15
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
!*  type integer and zero level nesting
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module testM

   contains

   subroutine test(a, b)
      integer, intent(in) :: a
      integer, optional :: b
      print *,a
      print *,present(b)
   end subroutine test

end module testM

program main

   use testM
   implicit none
   integer, pointer :: p

   nullify (p)
   call test (7001)
   call test (7001, p)

end program main

