!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-10-15
!*
!*  PRIMARY FUNCTIONS TESTED   : Assumed type parameters and dummy arguments
!*
!*  SECONDARY FUNCTIONS TESTED : test the contents
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
!*  Verify that external procedures (both functions and subroutines) with assumed
!*  type parameter dummy arguments have the correct values when invoked.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpATPDASimpleLengthContentExternalmod

  implicit none

  type dtChars (l)
     integer, len :: l
     character(l) :: ch
  end type dtChars

  type dtIntArray (l)
     integer, len :: l
     integer :: iarr(l)
  end type dtIntArray

  integer, save :: errorCount = 0

contains

  subroutine verifyLength(name,have,expect)
    character(*) :: name
    integer :: have, expect
    if (have /= expect) then
       print *, "  in ", name, ", arg%l is", have, "but should be", expect
       errorCount = errorCount + 1
    end if
  end subroutine verifyLength

end module dtpATPDASimpleLengthContentExternalmod


program dtpATPDASimpleLengthContentExternal

  use dtpATPDASimpleLengthContentExternalmod
  implicit none
  type (dtChars(5)) :: dc5
  type (dtChars(1)) :: dc1
  type (dtIntArray(9)) :: dia9
  type (dtIntArray(2)) :: dia2
  integer :: i

  interface
     subroutine dcSub(arg,expect)
       import :: dtChars
       type (dtChars(*)) :: arg
       integer, intent(in) :: expect
     end subroutine dcSub
     subroutine diaSub(arg,expect)
       import :: dtIntArray
       type (dtIntArray(*)) :: arg
       integer, intent(in) :: expect
     end subroutine diaSub
     function dcFun(arg,expect)
       import :: dtChars
       type (dtChars(*)) :: arg
       integer, intent(in) :: expect
       character(expect+2) :: dcFun
     end function dcFun
     function diaFun(arg,expect)
       import :: dtIntArray
       type (dtIntArray(*)) :: arg
       integer, intent(in) :: expect
       integer :: diaFun(expect+2)
     end function diaFun
  end interface


  dc5 % ch     = 'zyxwvuts'
  dia9 % iarr  = [124683553, 124683551, 124683547, 124683539, 124683523, 124683511, 124683473, 124683439, 124683379]

  dc1 % ch     = 'lmno'
  dia2 % iarr  = [123456791,123456761]

  call dcSub(dc5,5)
  call dcSub(dc1,1)

  call diaSub(dia9,9)
  call diaSub(dia2,2)

  print *, "dcFun(dc5):", dcFun(dc5,5)
  print *, "dcFun(dc1):", dcFun(dc1,1)

  print *, "diaFun(dia9):", diaFun(dia9,9)
  print *, "diaFun(dia2):", diaFun(dia2,2)

  if (errorCount /= 0) print *, errorCount, "errors encountered."
  print *, "end"

end program dtpATPDASimpleLengthContentExternal


subroutine dcSub(arg,expect)
  use :: dtpATPDASimpleLengthContentExternalmod
  implicit none
  type (dtChars(*)) :: arg
  integer, intent(in) :: expect
  print *, "dcSub: want:", expect, expect
  print *, "dcSub:   is:", arg % l, len(arg%ch)
  print *, "dcSub:  arg:", arg
  call verifyLength("dcSub.l", arg%l, expect)
  call verifyLength("dcSub", len(arg%ch), expect)
end subroutine dcSub

subroutine diaSub(arg,expect)
  use :: dtpATPDASimpleLengthContentExternalmod
  implicit none
  type (dtIntArray(*)) :: arg
  integer, intent(in) :: expect
  print *, "diaSub: want:", expect, expect
  print *, "diaSub:   is:", arg % l, size(arg%iarr)
  print *, "diaSub:  arg:", arg
  call verifyLength("diaSub.l", arg%l, expect)
  call verifyLength("diaSub", size(arg%iarr), expect)
end subroutine diaSub


function dcFun(arg,expect)
  use :: dtpATPDASimpleLengthContentExternalmod
  implicit none
  type (dtChars(*)) :: arg
  integer, intent(in) :: expect
  character(expect+2) :: dcFun
  if (arg%l /= expect) errorCount = errorCount + 1
  dcFun = repeat('.',len(dcFun))
  dcFun(2:expect+1) = arg%ch
end function dcFun

function diaFun(arg,expect)
  use :: dtpATPDASimpleLengthContentExternalmod
  implicit none
  type (dtIntArray(*)) :: arg
  integer, intent(in) :: expect
  integer :: diaFun(expect+2)

  if (arg%l /= expect) errorCount = errorCount + 1
  diaFun = -1
  diaFun(2:size(arg%iarr)+1)  = arg%iarr
end function diaFun