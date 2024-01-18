!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpATPDASimpleLengthInternal
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-10-15
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Assumed type parameters and dummy arguments
!*
!*  SECONDARY FUNCTIONS TESTED : test the length passed in
!*
!*  REFERENCE                  : Feature Number 357495
!*
!*  DRIVER STANZA              : xlf2003
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
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpATPDASimpleLengthInternalmod

  implicit none
  type dt (l)
     integer, len :: l
  end type dt

  type dtChars (l)
     integer, len :: l
     character(l) :: ch
  end type dtChars

  type dtIntArray (l)
     integer, len :: l
     integer :: iarr(l)
  end type dtIntArray

end module dtpATPDASimpleLengthInternalmod


program dtpATPDASimpleLengthInternal

  use dtpATPDASimpleLengthInternalmod
  implicit none
  type (dt(3)) :: d3
  type (dtChars(5)) :: dc5
  type (dtIntArray(9)) :: dia9
  integer :: i, errorCount

  errorCount = 0

  dc5 % ch = 'test'
  dia9 % iarr = [(i,i=1,9)]

  call dtSub(d3,3)
  call dcSub(dc5,5)
  call diaSub(dia9,9)

  print *, "dtFun(d3):", dtFun(d3,3)
  print *, "dcFun(dc5):", dcFun(dc5,5)
  print *, "diaFun(dia9):", diaFun(dia9,9)

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

  subroutine dtSub(arg,expect)
    type (dt(*)) :: arg
    integer, intent(in) :: expect
    print *, "dtSub:", arg % l
    call verifyLength("dtSub", arg%l, expect)
  end subroutine dtSub

  subroutine dcSub(arg,expect)
    type (dtChars(*)) :: arg
    integer, intent(in) :: expect
    print *, "dcSub:", arg % l, len(arg%ch)
    call verifyLength("dcSub", arg%l, expect)
  end subroutine dcSub

  subroutine diaSub(arg,expect)
    type (dtIntArray(*)) :: arg
    integer, intent(in) :: expect
    print *, "diaSub:", arg % l, size(arg%iarr)
    call verifyLength("diaSub", arg%l, expect)
  end subroutine diaSub


  integer function dtFun(arg,expect)
    type (dt(*)) :: arg
    integer, intent(in) :: expect
    if (arg%l /= expect) errorCount = errorCount + 1
    dtFun = arg % l
  end function dtFun

  integer function dcFun(arg,expect)
    type (dtChars(*)) :: arg
    integer, intent(in) :: expect
    if (arg%l /= expect) errorCount = errorCount + 1
    dcFun = arg % l + 1000 * len(arg%ch)
  end function dcFun

  integer function diaFun(arg,expect)
    type (dtIntArray(*)) :: arg
    integer, intent(in) :: expect
    if (arg%l /= expect) errorCount = errorCount + 1
    diaFun = arg % l + 1000 * size(arg%iarr)
  end function diaFun


end program dtpATPDASimpleLengthInternal
