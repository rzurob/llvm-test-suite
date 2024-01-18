!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : caflock02f.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Ren Jian Gang
!*  DATE                       : May 08, 2011
!*  ORIGIN                     : Compiler Development, IBM CDL
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 387873
!*
!*  DRIVER STANZA              :
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

  type(lock_type), private, save :: stack_lock[*]
  type(task), private, save :: stack(100)[*]
  integer, private, save :: stack_size[*]

  contains
    subroutine get_task(job)
      ! Get a task from my stack
      type(task), intent(out) :: job
      character(5) :: str

      lock(stack_lock[this_image()])
      job = stack(stack_size)
      stack_size = stack_size - 1
      runlock(stack_lock[this_image()])
    end subroutine get_task
	
	subroutine put_task(job,image)
      ! Put a task on the stack of image
      type(task),intent(in) :: job
      integer,intent(in) :: image
  
      lock(stack_lock[image])
      stack_size[image] = stack_size[image] + 1
      stack(stack_size[image])[image] = job
      runlock(stack_lock[image])
    end subroutine put_task
end module stack_manager

program caflock02f
  use stack_manager

  type(task) :: tk

  call get_task(tk)
  print *, "get_task: ", tk%i
  call put_task(tk, this_image())
  print *, "put_task: ", tk%i
end program
