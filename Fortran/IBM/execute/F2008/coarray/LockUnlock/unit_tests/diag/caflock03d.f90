!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : caflockdiag3.f
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
!*  CAF: LOCK/UNLOCK statements
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program caflock03d
  type lock_type
    integer :: i
  integer :: j
  end type

  type(lock_type), save :: stack_lock[*]

  integer :: i
  
  i = 5

  call lock_add(i)
  call lock_sub(i)
  
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
      var = var - 1
      runlock(stack_lock)
    end
end program
