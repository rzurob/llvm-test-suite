!*  ===================================================================
!*
!*  TEST CASE NAME             : caflock04f.f
!*
!*  DATE                       : May 08, 2011
!*  ORIGIN                     : Compiler Development, IBM CDL
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 387873
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  CAF: LOCK/UNLOCK statements
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module lock_manager
  use, intrinsic :: iso_fortran_env, only: lock_type

  type(lock_type), private, save :: stack_lock(10)[*]

  contains
    subroutine lock_add(var, j)
      integer, intent(out) :: var
      integer, intent(in) :: j

      lock(stack_lock(j)[5])
      var = var + 1
      runlock(stack_lock(j)[5])
    end

    subroutine lock_sub(var, j)
      integer, intent(out) :: var

      lock(stack_lock(j + 3)[5])
      var = var + 1
      runlock(stack_lock(j + 3)[5])
    end
end module lock_manager

program p
  use lock_manager

  integer :: i = 5
  integer :: j = 6

  call lock_add(i, j)
  print *, "Add: ", i

  j = 3

  call lock_sub(i, j)
  print *, "Sub: ", i
end program
