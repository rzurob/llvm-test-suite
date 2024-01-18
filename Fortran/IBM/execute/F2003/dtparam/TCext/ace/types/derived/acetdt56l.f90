! GM DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/ace/types/derived/acetdt56.f

!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt56l
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-23 (original: 2006-11-23)
!*  ORIGIN                     : Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : function, return
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  Test use of AC as function return value (RHS of assignment to function
!*  name).  Include recursive functions.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt56lmod

  implicit none
  type derived(l1,k1)    ! (20,4)
     integer, kind :: k1
     integer, len  :: l1
     integer(k1)   :: ival
   contains
     procedure :: derivedEquals
     generic :: operator(.eq.) => derivedEquals
  end type derived

contains

  elemental logical function derivedEquals(this, that)
    class (derived(*,4)), intent(in) :: this, that
    derivedEquals = this % ival == that % ival
  end function derivedEquals

  function func(arg)
    integer(4) :: arg
    type (derived(20,4)) :: func(arg)
    integer(4) :: i
    func = [derived(20,4):: (derived(20,4)(i), i=arg,1,-1)]
  end function func

  recursive function rfunc(arg) result(retval)
    integer(4) :: arg
    type (derived(20,4)) :: retval(arg)
    if (arg <= 0) then
       retval = [derived(20,4)::]
    else
       retval = [derived(20,4):: derived(20,4)(arg), rfunc(arg-1)]
    end if
  end function rfunc

  subroutine test(arr)
    type (derived(*,4)) :: arr(:)
    print *, "test:", size(arr), "[", arr, "]"
  end subroutine test

end module acetdt56lmod


program acetdt56l

  use acetdt56lmod
  implicit none

  type (derived(20,4)) :: arr(4)
  integer :: i

  print *, "func(0):", func(0)
  print *, "rfunc(0):", rfunc(0)

  print *, "func(1):", func(1)
  print *, "rfunc(1):", rfunc(1)

  print *, "func(3):", func(3)
  print *, "rfunc(3):", rfunc(3)

  ! Try it in an expression:
  print *, "compare:", rfunc(3) == func(3)

  ! and in an assignment:
  arr = [derived(20,4):: (derived(20,4)(i), i=4,1,-1)]
  print *, arr

  arr(1:1) = func(1)
  arr(2:4) = func(3)
  print *, arr

  arr(1:1) = rfunc(1)
  arr(2:4) = rfunc(3)
  print *, arr

  ! and in a subroutine call:
  call test (func(0))
  call test (rfunc(0))

  call test (func(1))
  call test (rfunc(1))

  call test (func(3))
  call test (rfunc(3))

end program acetdt56l
