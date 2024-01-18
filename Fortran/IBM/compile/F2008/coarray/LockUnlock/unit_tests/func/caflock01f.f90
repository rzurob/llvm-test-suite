!*  ===================================================================
!*
!*  TEST CASE NAME             : caflock01f.f
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

  type(lock_type), private, save :: stack_lock[*]

  contains
    subroutine lock_add(var)
      integer, intent(out) :: var

      lock(stack_lock)
      var = var + 1
      runlock(stack_lock)
    end

    subroutine lock_sub(var)
      integer, intent(out) :: var

      lock(stack_lock)
      var = var + 1
      runlock(stack_lock)
    end
end module lock_manager

program p
  use lock_manager

  integer :: i = 5

  call lock_add(i)
  print *, "Add: ", i
  call lock_sub(i)
  print *, "Sub: ", i
end program
