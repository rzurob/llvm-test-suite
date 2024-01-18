!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : caflock06f.f
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
	  integer :: i1, i2
	  character(50) :: c1, c2
	  logical :: l
	
      ! Get a task from my stack
      type(task), intent(out) :: job
      character(5) :: str

      lock(stack_lock[this_image()], stat=i1, errmsg=c1, acquired_lock=l)
      job = stack(stack_size)
      stack_size = stack_size - 1
      runlock(stack_lock[this_image()], stat=i2, errmsg=c2)
	  
	  print *, "i1 = ", i1
	  print *, "i2 = ", i2
	  print *, "c1 = ", trim(c1)
	  print *, "c2 = ", trim(c2)
    end subroutine get_task
	
	subroutine put_task(job,image)
	  integer :: i1, i2
	  character(50) :: c1, c2
	  logical :: l
	
      ! Put a task on the stack of image
      type(task),intent(in) :: job
      integer,intent(in) :: image
  
      lock(stack_lock[image], stat=i1, errmsg=c1, acquired_lock=l)
      stack_size[image] = stack_size[image] + 1
      stack(stack_size[image])[image] = job
      runlock(stack_lock[image], stat=i2, errmsg=c2)
	  
	  print *, "i1 = ", i1
	  print *, "i2 = ", i2
	  print *, "c1 = ", trim(c1)
	  print *, "c2 = ", trim(c2)
    end subroutine put_task
end module stack_manager

program caflockfunc2
  use stack_manager

  type(task) :: tk

  call get_task(tk)
  print *, "get_task: ", tk%i
  call put_task(tk, this_image())
  print *, "put_task: ", tk%i
end program
