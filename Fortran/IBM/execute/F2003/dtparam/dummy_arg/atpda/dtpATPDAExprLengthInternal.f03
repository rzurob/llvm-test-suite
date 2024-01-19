!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-10-15
!*
!*  PRIMARY FUNCTIONS TESTED   : Assumed type parameters and dummy arguments
!*
!*  SECONDARY FUNCTIONS TESTED : test the length passed in
!*
!*  REFERENCE                  : Feature Number 357495
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that internal procedures (both functions and subroutines) with assumed
!*  type parameter dummy arguments have the correct values when invoked.
!*  Here, the length parameter appears in an expression rather than by itself.
!*  (Otherwise, this is the same as dtpATPDASimpleLengthInternal.)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpATPDAExprLengthInternalmod

  implicit none

  type dtChars (l)
     integer, len :: l
     character(l+2) :: chPlus
     character(2*l) :: chMult
     character(min(l,2)) :: chIntrin
  end type dtChars

  type dtIntArray (l)
     integer, len :: l
     integer :: iarrMinus(l-2)
     integer :: iarrSquare(l**2)
     integer :: iarrIntrin(dim(4,l))
  end type dtIntArray

end module dtpATPDAExprLengthInternalmod


program dtpATPDAExprLengthInternal

  use dtpATPDAExprLengthInternalmod
  implicit none
  type (dtChars(5)) :: dc5
  type (dtIntArray(9)) :: dia9
  integer :: i, errorCount

  errorCount = 0

  dc5 % chPlus      = 'zyxwvuts'     !  7 = 5+2; should be "zyxwvut"
  dc5 % chMult      = 'abcdefghijk'  ! 10 = 2*5; should be "abcdefghij"
  dc5 % chIntrin    = 'pqr'          !  2 = min(5,2);      "pq"
  dia9 % iarrMinus  = [(i,i=1,7)]    !  7 = 9-2
  dia9 % iarrSquare = [(81-i,i=1,81)]! 81 = 9**2
  dia9 % iarrIntrin = [(i*2,i=1,0)]  !  0 = dim(4,9) = dim(-5)

  call dcSub(dc5,5)
  call diaSub(dia9,9)

  print *, "dcFun(dc5):", dcFun(dc5,5)
  print *, "       want", 5 + 100 * (5+2) + 10000 * (2*5) + 1000000 * min(5,2)
  print *, "diaFun(dia9):", diaFun(dia9,9)
  print *, "       want", 9 + 100 * (9-2) + 10000 * (9**2) + 1000000 * dim(4,9)

  if (errorCount /= 0) print *, errorCount, "errors encountered."
  print *, "end"

contains


  subroutine verifyLength(name,have,expect)
    character(*) :: name
    integer :: have, expect
    if (have /= expect) then
       print *, "  in ", name, ", arg%l is", have, "but should be", expect
       errorCount = errorCount + 1
    end if
  end subroutine verifyLength

  subroutine dcSub(arg,expect)
    type (dtChars(*)) :: arg
    integer, intent(in) :: expect
    print *, "dcSub: want", expect, (expect+2), (2*expect), min(expect,2)
    print *, "dcSub:   is", arg % l, len(arg%chPlus), len(arg%chMult), len(arg%chIntrin)
    call verifyLength("dcSub.l", arg%l, expect)
    call verifyLength("dcSub +", len(arg%chPlus), (expect+2))
    call verifyLength("dcSub *", len(arg%chMult), (2*expect))
    call verifyLength("dcSub I", len(arg%chIntrin), min(expect,2))
  end subroutine dcSub

  subroutine diaSub(arg,expect)
    type (dtIntArray(*)) :: arg
    integer, intent(in) :: expect
    print *, "diaSub: want", expect, (expect-2), (expect**2), dim(4,expect)
    print *, "diaSub:   is", arg % l, size(arg%iarrMinus), size(arg%iarrSquare), size(arg%iarrIntrin)
    call verifyLength("diaSub.l", arg%l, expect)
    call verifyLength("diaSub -", size(arg%iarrMinus), (expect-2))
    call verifyLength("diaSub**", size(arg%iarrSquare), (expect**2))
    call verifyLength("diaSub I", size(arg%iarrIntrin), dim(4,expect))
  end subroutine diaSub


  integer function dcFun(arg,expect)
    type (dtChars(*)) :: arg
    integer, intent(in) :: expect
    if (arg%l /= expect) errorCount = errorCount + 1
    dcFun = arg % l + 100 * len(arg%chPlus) + 10000 * len(arg%chMult) + 1000000 * len(arg%chIntrin)
  end function dcFun

  integer function diaFun(arg,expect)
    type (dtIntArray(*)) :: arg
    integer, intent(in) :: expect
    if (arg%l /= expect) errorCount = errorCount + 1
    diaFun = arg % l + 100 * size(arg%iarrMinus) + 10000 * size(arg%iarrSquare) + 1000000 * size(arg%iarrIntrin)
  end function diaFun


end program dtpATPDAExprLengthInternal
