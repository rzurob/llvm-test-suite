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
!*  type integer and one level nesting
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module testM

   contains

   subroutine test(a, b)
      integer, intent(in) :: a
      integer, optional, intent(inout) :: b
      call test_1(a, b)
   end subroutine test

   subroutine test_1(a, b)
      integer, intent(in) :: a
      integer, optional, intent(inout) :: b
      print *,a
      print *,present(b)
      if (present(b)) then
         print *,b
      end if
   end subroutine test_1

end module testM

program main

   use testM
   implicit none
   integer, allocatable :: p

   allocate(p)
   deallocate(p)

   call test (7001)
   call test (7001, p)

   allocate(p)
   p = 7005
   call test (7003, p)
   deallocate(p)

end program main
