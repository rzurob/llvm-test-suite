!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : caflockdiag4.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Ren Jian Gang
!*  DATE                       : June 12, 2011
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
!*  CAF: LOCK/UNLOCK statements with specifiers
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program caflock04d
  use, intrinsic :: iso_fortran_env, only: lock_type

  type(lock_type), save :: stack_lock[*]

  integer :: i
  
  i = 5

  call lock_add(i)
  call lock_sub(i)
  
  contains
    subroutine lock_add(var)
      integer, intent(out) :: var
      integer :: s1, s2
      character(10) :: c1, c2

      lock(stack_lock, stat=s1, stat=s2, errmsg=c1, errmsg=c2)
      var = var + 1
      runlock(stack_lock, stat=c1, errmsg=s1)
    end

    subroutine lock_sub(var)
      integer, intent(out) :: var
      integer :: s1, s2
      character(10) :: c1, c2
	    logical :: l1, l2

      lock(stack_lock, stat=s1, errmsg=c2, acquired_lock=l1, acquired_lock=l2)
      var = var - 1
      runlock(stack_lock, acquired_lock=l1)
  
      lock(stack_lock, stat=s1, errmsg=c2, acquired_lock=c2)
      var = var - 1
      runlock(stack_lock, stat=s1, errmsg=c2, acquired_lock=l1)
    end
end program
