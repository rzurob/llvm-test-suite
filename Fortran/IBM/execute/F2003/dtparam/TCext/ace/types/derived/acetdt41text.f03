! GM DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/ace/types/derived/acetdt41t.f

!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-17 (original: 2006-10-18)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
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
!*  Invoke two different subroutines with arrays of derived type of different
!*  ranks.  One subroutine treats the dummy argument arrays as deferred-shape
!*  arrays, and the other as assumed-size arrays.  Within each subroutine,
!*  construct AC's and attempt to assign them, print them, and call procedures
!*  with them as arguments, including intrinsics and elemental functions.  In
!*  most cases below which use assumed-size arrays, we are not allowed to use
!*  "whole array references" (i.e., giving just the array name), so we have to
!*  use an implied do.  We also use an implied-do with the deferred-shape
!*  arrays, to test its use.
!*
!*  Related test cases:
!*  diag/synt/acesynt41[acp]d -- diagnostic
!*  -- tests use of assumed-size arrays in AC's (assignment, call, print)
!*  types/intrinsics/acetint41[cilrz]
!*  -- tests correct use of dummy-argument arrays in AC's (assignment, call,
!*  print)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

program acetdt41text

  implicit none

  type derived(k1)    ! (4)
     integer, kind :: k1
     integer(k1)   :: field = 1
  end type derived

  type(derived(4)) :: rank1(2), rank2(2,2)

  rank1 = (/derived(4) :: derived(4)(2),derived(4)(3)/)
  rank2 = reshape([derived(4) :: derived(4)(4),derived(4)(5),derived(4)(6),derived(4)(7)],[2,2])
  call buildWithDeferred (derived(4)(1), rank1, rank2)

contains

  subroutine buildWithDeferred(arg0, arg1, arg2)
    type(derived(4)) :: arg0, arg1(:), arg2(:,:)
    type(derived(4)) :: array(1 + 2*size(arg1) + size(arg2)), val
    integer    :: i
    array = [derived(4) :: arg0, arg1, arg2, (arg1(i),i=1,size(arg1))]
    print *, "D1:", array
    print *, "D2:", (/ derived(4) :: arg0, arg1, arg2, (arg1(i),i=1,size(arg1)) /)
    print *, "D3:", size([derived(4) :: arg1]), size([derived(4) :: arg2]), &
                    ubound([derived(4) :: arg1]), maxval([integer :: arg2 % field]), &
                    maxval(reshape([integer :: arg2 % field],[integer:: 2, 2]),2)
    call subTest("D4", [derived(4) :: arg2, arg1])
    print *, "D5:", [derived(4) ::arg0, arg1, arg1(size(arg1)), (arg1(i),i=1,size(arg1)), &
                               arg2, arg2(ubound(arg2,1),ubound(arg2,2))]
    val = funTest([derived(4) ::  arg0, arg1, arg1(size(arg1)), (arg1(i),i=1,size(arg1)), &
                               arg2, arg2(ubound(arg2,1),ubound(arg2,2))], 5)
    print *, "D6:", val
    print *, "D7:", eTest([derived(4) :: arg0, arg1, arg1(size(arg1)), (arg1(i),i=1,size(arg1)), &
                                      arg2, arg2(ubound(arg2,1),ubound(arg2,2))])
    print *, "D8:", eTest(reshape([derived(4) :: arg2],[integer:: 2, 2]))
    call buildWithAssumed  (arg0, size(arg1), arg1, ubound(arg2,1), lbound(arg2,2), size(arg2), arg2, arg2)

  end subroutine buildWithDeferred

  subroutine buildWithAssumed(arg0, s1, arg1, u1, l2, s2, arg2, arg2a)
    integer    :: s1, s2, l2, u1, i, j
    type(derived(4)) :: arg0, arg1(*), arg2(u1,*), arg2a(u1,l2:*)
    type(derived(4)) :: array(s1 + 2*s2 + 4), val
    array = (/derived(4) :: arg0, (arg1(i),i=1,s1), arg1(s1), &
                             ((arg2(i,j),i=1,u1),j=1,s2/u1), arg2(u1,s2/u1), &
                             ((arg2a(i,j),i=1,u1),j=l2,s2/u1), arg2a(u1,l2) /)
    print *, "A1:", array
    print *, "A2:", [derived(4) :: arg0, (arg1(i),i=1,s1), arg1(s1), &
                             ((arg2(i,j),i=1,u1),j=1,s2/u1), arg2(u1,s2/u1), &
                             ((arg2a(i,j),i=1,u1),j=l2,s2/u1), arg2a(u1,l2)]
    print *, "A3:", ubound([derived(4) :: arg0, (arg1(i),i=1,s1), arg1(s1), &
                             ((arg2(i,j),i=1,u1),j=1,s2/u1), arg2(u1,s2/u1), &
                             ((arg2a(i,j),i=1,u1),j=l2,s2/u1), arg2a(u1,l2)])

    call subTest("A4", (/derived(4) :: arg0, (arg1(i),i=1,s1), arg1(s1), &
                             ((arg2(i,j),i=1,u1),j=1,s2/u1), arg2(u1,s2/u1), &
                             ((arg2a(i,j),i=1,u1),j=l2,s2/u1), arg2a(u1,l2) /))
    val = funTest([derived(4) :: arg0, (arg1(i),i=1,s1), arg1(s1), &
                             ((arg2(i,j),i=1,u1),j=1,s2/u1), arg2(u1,s2/u1), &
                             ((arg2a(i,j),i=1,u1),j=l2,s2/u1), arg2a(u1,l2)], 5)
    print *, "A5:", val
    print *, "A6:", eTest([derived(4) :: arg0, (arg1(i),i=1,s1), arg1(s1), &
                             ((arg2(i,j),i=1,u1),j=1,s2/u1), arg2(u1,s2/u1), &
                             ((arg2a(i,j),i=1,u1),j=l2,s2/u1), arg2a(u1,l2)])
  end subroutine buildWithAssumed

  subroutine subTest (label, arg)
    character(2) :: label
    type(derived(4)) :: arg(:)
    print *, label, ":", size(arg), arg
  end subroutine subTest

  elemental integer function eTest (arg)
    type(derived(4)), intent(in) :: arg
    eTest = arg % field + 1
  end function eTest

  type(derived(4)) function funTest(arg, inx)
    integer    :: inx
    type(derived(4)) :: arg(:)
    funTest = arg(inx)
  end function funTest


end program acetdt41text
