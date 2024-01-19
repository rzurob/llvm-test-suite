!*  ===================================================================
!*
!*  DATE                       : May 08, 2011
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

module stack_manager
  use, intrinsic :: iso_fortran_env, only: lock_type

  type task
    integer :: i = 9
  end type

  type(lock_type), private, save :: dimen_lock[-5:2, *]
  type(task), private, save :: stack(100)[*]
  integer, private, save :: stack_size[*]

  contains
    subroutine get_task(job)
      ! Get a task from my stack
      type(task), intent(out) :: job
      character(5) :: str

      lock(dimen_lock[-3, 2])
      job = stack(stack_size)
      stack_size = stack_size - 1
      runlock(dimen_lock[-3, 2])
    end subroutine get_task

	subroutine put_task(job,image)
      ! Put a task on the stack of image
      type(task),intent(in) :: job
      integer,intent(in) :: image

      lock(dimen_lock[image, image + 1])
      stack_size[image] = stack_size[image] + 1
      stack(stack_size[image])[image] = job
      runlock(dimen_lock[image, image + 1])
    end subroutine put_task
end module stack_manager

program caflock03f
  use stack_manager

  type(task) :: tk

  call get_task(tk)
  print *, "get_task: ", tk%i
  call put_task(tk, this_image())
  print *, "put_task: ", tk%i
end program
