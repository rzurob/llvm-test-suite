!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetint41l
!*
!*  DATE                       : 2006-10-18
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : AC containing dummy arguments, including arrays (logicals)
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
!*  Invoke two different subroutines with logical arrays of different ranks.
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
!*  types/intrinsics/acetint41[cirz]
!*    -- tests assignment, call, print of dummy-argument arrays in AC's (char,int,real,complex)
!*  types/derived/acetdt41t
!*    -- tests assignment, call, print of dummy-argument arrays in AC's (derived type)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint41l

  implicit none
  logical(4) :: rank1(2), rank2(2,2)

  rank1 = (/logical(4) :: .FALSE., .TRUE./)
  rank2 = reshape([logical(4) :: .FALSE., .TRUE., .TRUE., .FALSE.],[2,2])
  call buildWithDeferred (.TRUE., rank1, rank2)

contains

  subroutine buildWithDeferred(arg0, arg1, arg2)
    logical(4) :: arg0, arg1(:), arg2(:,:)
    logical(4) :: array(1 + 2*size(arg1) + size(arg2)), val
    integer    :: i
    array = [logical(4) :: arg0, arg1, arg2, (arg1(i),i=1,size(arg1))]
    print *, "D1:", array
    print *, "D2:", (/ logical(4) :: arg0, arg1, arg2, (arg1(i),i=1,size(arg1)) /)
    print *, "D3:", size([logical(4) :: arg1]), size([logical(4) :: arg2]), &
             ubound([logical(4) :: arg1]), count([logical(4) :: arg2]), &
             count(reshape([logical(4) :: arg2],[integer:: 2, 2]))
    call subTest("D4", [logical(4) :: arg2, arg1])
    print *, "D5:", [logical(4) ::       arg0, arg1, arg1(size(arg1)), (arg1(i),i=1,size(arg1)), &
                                  arg2, arg2(ubound(arg2,1),ubound(arg2,2))]
    val = funTest([logical(4) ::  arg0, arg1, arg1(size(arg1)), (arg1(i),i=1,size(arg1)), &
                                  arg2, arg2(ubound(arg2,1),ubound(arg2,2))], 5)
    print *, "D6:", val
    print *, "D7:", eTest([logical(4) :: arg0, arg1, arg1(size(arg1)), (arg1(i),i=1,size(arg1)), &
                                  arg2, arg2(ubound(arg2,1),ubound(arg2,2))])
    print *, "D8:", eTest(reshape([logical(4) :: arg2],[integer:: 2, 2]))
    call buildWithAssumed  (arg0, size(arg1), arg1, ubound(arg2,1), lbound(arg2,2), size(arg2), arg2, arg2)

  end subroutine buildWithDeferred

  subroutine buildWithAssumed(arg0, s1, arg1, u1, l2, s2, arg2, arg2a)
    integer    :: s1, s2, l2, u1, i, j
    logical(4) :: arg0, arg1(*), arg2(u1,*), arg2a(u1,l2:*)
    logical(4) :: array(s1 + 2*s2 + 4), val
    array = (/logical(4) :: arg0, (arg1(i),i=1,s1), arg1(s1), &
                      ((arg2(i,j),i=1,u1),j=1,s2/u1), arg2(u1,s2/u1), &
                      ((arg2a(i,j),i=1,u1),j=l2,s2/u1), arg2a(u1,l2) /)
    print *, "A1:", array
    print *, "A2:", [logical(4) :: arg0, (arg1(i),i=1,s1), arg1(s1), &
                      ((arg2(i,j),i=1,u1),j=1,s2/u1), arg2(u1,s2/u1), &
                      ((arg2a(i,j),i=1,u1),j=l2,s2/u1), arg2a(u1,l2)]
    print *, "A3:", count([logical(4) :: arg0, (arg1(i),i=1,s1), arg1(s1), &
                      ((arg2(i,j),i=1,u1),j=1,s2/u1), arg2(u1,s2/u1), &
                      ((arg2a(i,j),i=1,u1),j=l2,s2/u1), arg2a(u1,l2)])

    call subTest("A4", (/logical(4) :: arg0, (arg1(i),i=1,s1), arg1(s1), &
                         ((arg2(i,j),i=1,u1),j=1,s2/u1), arg2(u1,s2/u1), &
                         ((arg2a(i,j),i=1,u1),j=l2,s2/u1), arg2a(u1,l2) /))
    val = funTest([logical(4) :: arg0, (arg1(i),i=1,s1), arg1(s1), &
                      ((arg2(i,j),i=1,u1),j=1,s2/u1), arg2(u1,s2/u1), &
                      ((arg2a(i,j),i=1,u1),j=l2,s2/u1), arg2a(u1,l2)], 5)
    print *, "A5:", val
    print *, "A6:", eTest([logical(4) :: arg0, (arg1(i),i=1,s1), arg1(s1), &
                      ((arg2(i,j),i=1,u1),j=1,s2/u1), arg2(u1,s2/u1), &
                      ((arg2a(i,j),i=1,u1),j=l2,s2/u1), arg2a(u1,l2)])
  end subroutine buildWithAssumed

  subroutine subTest (label, arg)
    character(2) :: label
    logical(4) :: arg(:)
    print *, label, ":", size(arg), arg
  end subroutine subTest

  elemental logical(4) function eTest (arg)
    logical(4), intent(in) :: arg
    eTest = .not. arg
  end function eTest

  logical(4) function funTest(arg, inx)
    integer    :: inx
    logical(4) :: arg(:)
    funTest = arg(inx)
  end function funTest


end program acetint41l
