!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-10-18
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : AC containing dummy arguments, including arrays (derived types)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : dummy argument, array constructor
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Invoke two different subroutines with arrays of derived type of different ranks.
!*  One subroutine treats the dummy argument arrays as deferred-shape arrays, and
!*  the other as assumed-size arrays.  Within each subroutine, construct AC's and
!*  attempt to assign them, print them, and call procedures with them as arguments,
!*  including intrinsics and elemental functions.  In most cases below which use
!*  assumed-size arrays, we are not allowed to use "whole array references" (i.e.,
!*  giving just the array name), so we have to use an implied do.  We also use an
!*  implied-do with the deferred-shape arrays, to test its use.
!*
!*  Related test cases:
!*  diag/synt/acesynt41[acp]d -- diagnostic
!*    -- tests use of assumed-size arrays in AC's (assignment, call, print)
!*  types/intrinsics/acetint41[cilrz]
!*    -- tests correct use of dummy-argument arrays in AC's (assignment, call, print)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetdt41t

  implicit none

  type derived
     integer :: field = 1
  end type derived

  type(derived) :: rank1(2), rank2(2,2)

  rank1 = (/derived :: derived(2),derived(3)/)
  rank2 = reshape([derived :: derived(4),derived(5),derived(6),derived(7)],[2,2])
  call buildWithDeferred (derived(1), rank1, rank2)

contains

  subroutine buildWithDeferred(arg0, arg1, arg2)
    type(derived) :: arg0, arg1(:), arg2(:,:)
    type(derived) :: array(1 + 2*size(arg1) + size(arg2)), val
    integer    :: i
    array = [derived :: arg0, arg1, arg2, (arg1(i),i=1,size(arg1))]
    print *, "D1:", array
    print *, "D2:", (/ derived :: arg0, arg1, arg2, (arg1(i),i=1,size(arg1)) /)
    print *, "D3:", size([derived :: arg1]), size([derived :: arg2]), &
                    ubound([derived :: arg1]), maxval([integer :: arg2 % field]), &
                    maxval(reshape([integer :: arg2 % field],[integer:: 2, 2]),2)
    call subTest("D4", [derived :: arg2, arg1])
    print *, "D5:", [derived ::arg0, arg1, arg1(size(arg1)), (arg1(i),i=1,size(arg1)), &
                               arg2, arg2(ubound(arg2,1),ubound(arg2,2))]
    val = funTest([derived ::  arg0, arg1, arg1(size(arg1)), (arg1(i),i=1,size(arg1)), &
                               arg2, arg2(ubound(arg2,1),ubound(arg2,2))], 5)
    print *, "D6:", val
    print *, "D7:", eTest([derived :: arg0, arg1, arg1(size(arg1)), (arg1(i),i=1,size(arg1)), &
                                      arg2, arg2(ubound(arg2,1),ubound(arg2,2))])
    print *, "D8:", eTest(reshape([derived :: arg2],[integer:: 2, 2]))
    call buildWithAssumed  (arg0, size(arg1), arg1, ubound(arg2,1), lbound(arg2,2), size(arg2), arg2, arg2)

  end subroutine buildWithDeferred

  subroutine buildWithAssumed(arg0, s1, arg1, u1, l2, s2, arg2, arg2a)
    integer    :: s1, s2, l2, u1, i, j
    type(derived) :: arg0, arg1(*), arg2(u1,*), arg2a(u1,l2:*)
    type(derived) :: array(s1 + 2*s2 + 4), val
    array = (/derived :: arg0, (arg1(i),i=1,s1), arg1(s1), &
                             ((arg2(i,j),i=1,u1),j=1,s2/u1), arg2(u1,s2/u1), &
                             ((arg2a(i,j),i=1,u1),j=l2,s2/u1), arg2a(u1,l2) /)
    print *, "A1:", array
    print *, "A2:", [derived :: arg0, (arg1(i),i=1,s1), arg1(s1), &
                             ((arg2(i,j),i=1,u1),j=1,s2/u1), arg2(u1,s2/u1), &
                             ((arg2a(i,j),i=1,u1),j=l2,s2/u1), arg2a(u1,l2)]
    print *, "A3:", ubound([derived :: arg0, (arg1(i),i=1,s1), arg1(s1), &
                             ((arg2(i,j),i=1,u1),j=1,s2/u1), arg2(u1,s2/u1), &
                             ((arg2a(i,j),i=1,u1),j=l2,s2/u1), arg2a(u1,l2)])

    call subTest("A4", (/derived :: arg0, (arg1(i),i=1,s1), arg1(s1), &
                             ((arg2(i,j),i=1,u1),j=1,s2/u1), arg2(u1,s2/u1), &
                             ((arg2a(i,j),i=1,u1),j=l2,s2/u1), arg2a(u1,l2) /))
    val = funTest([derived :: arg0, (arg1(i),i=1,s1), arg1(s1), &
                             ((arg2(i,j),i=1,u1),j=1,s2/u1), arg2(u1,s2/u1), &
                             ((arg2a(i,j),i=1,u1),j=l2,s2/u1), arg2a(u1,l2)], 5)
    print *, "A5:", val
    print *, "A6:", eTest([derived :: arg0, (arg1(i),i=1,s1), arg1(s1), &
                             ((arg2(i,j),i=1,u1),j=1,s2/u1), arg2(u1,s2/u1), &
                             ((arg2a(i,j),i=1,u1),j=l2,s2/u1), arg2a(u1,l2)])
  end subroutine buildWithAssumed

  subroutine subTest (label, arg)
    character(2) :: label
    type(derived) :: arg(:)
    print *, label, ":", size(arg), arg
  end subroutine subTest

  elemental integer function eTest (arg)
    type(derived), intent(in) :: arg
    eTest = arg % field + 1
  end function eTest

  type(derived) function funTest(arg, inx)
    integer    :: inx
    type(derived) :: arg(:)
    funTest = arg(inx)
  end function funTest


end program acetdt41t
