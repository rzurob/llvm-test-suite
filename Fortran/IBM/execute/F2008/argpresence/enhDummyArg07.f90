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
!*  type integer and two level nesting
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module testM

   contains

   subroutine test(a, b)
      integer, intent(in) :: a
      integer, optional :: b
      call test_1(a, b)
   end subroutine test

   subroutine test_1(a, b)
      integer, intent(in) :: a
      integer, optional :: b
      call test_2(a, b)
   end subroutine test_1

   subroutine test_2(a, b)
      integer, intent(in) :: a
      integer, optional :: b
      print *,a
      print *,present(b)
   end subroutine test_2

end module testM

program main

   use testM
   implicit none
   integer, allocatable :: p

   call test (7003)
   call test (7003, p)

end program main

