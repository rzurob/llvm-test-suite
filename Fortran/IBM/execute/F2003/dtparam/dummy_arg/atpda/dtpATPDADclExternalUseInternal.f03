!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-10-15
!*
!*  PRIMARY FUNCTIONS TESTED   : Assumed type parameters and dummy arguments
!*
!*  SECONDARY FUNCTIONS TESTED : declare in external procedures, use in internal procedures
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
!*  Declare types in external procedures and invoke internal procedures on instances of the types.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpATPDADclExternalUseInternalMod
  implicit none
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

end module dtpATPDADclExternalUseInternalMod


program dtpATPDADclExternalUseInternal

  use :: dtpATPDADclExternalUseInternalMod
  implicit none
  integer :: i

  call extDTSub
  call extDCSub
  call extDIASub

  if (errorCount /= 0) print *, errorCount, "errors encountered."
  print *, "end"

end program dtpATPDADclExternalUseInternal


subroutine extDTSub
  use :: dtpATPDADclExternalUseInternalMod
  implicit none
  type dt(l)
     integer, len :: l
  end type dt
  type (dt(99))     :: dt99
  type (dt(0))      :: dt0

  call dtSub(dt99,99)
  call dtSub(dt0,0)
  print *, "dtFun(dt99):", dtFun(dt99,99)
  print *, "dtFun(dt0):", dtFun(dt0,0)

contains

  subroutine dtSub(arg,expect)
    type (dt(*)) :: arg
    integer, intent(in) :: expect
    call verifyLength("dt.l", arg%l, expect)
  end subroutine dtSub

  integer function dtFun(arg,expect)
    type (dt(*)) :: arg
    integer, intent(in) :: expect
    if (arg%l /= expect) errorCount = errorCount + 1
    dtFun = arg % l - expect
  end function dtFun

end subroutine extDTSub


subroutine extDCSub
  use :: dtpATPDADclExternalUseInternalMod
  implicit none
  type dtChars (l)
     integer, len :: l
     character(l)   :: ch
     character(l+2) :: chPlus
     character(2*l) :: chMult
     character(min(l,2)) :: chIntrin
  end type dtChars
  type (dtChars(5)) :: dc5
  type (dtChars(1)) :: dc1

  dc5 % ch          = 'aeiouy'
  dc5 % chPlus      = 'zyxwvuts'
  dc5 % chMult      = 'abcdefghijk'
  dc5 % chIntrin    = 'pqr'
  dc1 % ch          = 'qrs'
  dc1 % chPlus      = 'lmno'
  dc1 % chMult      = 'kjih'
  dc1 % chIntrin    = 'gfe'

  call dcSub(dc5,5)
  call dcSub(dc1,1)

  print *, "dcFun(dc5) 1:", dcFun(dc5,5,1)
  print *, "dcFun(dc5) 2:", dcFun(dc5,5,2)
  print *, "dcFun(dc5) 3:", dcFun(dc5,5,3)
  print *, "dcFun(dc5) 4:", dcFun(dc5,5,4)

  print *, "dcFun(dc1) 1:", dcFun(dc1,1,1)
  print *, "dcFun(dc1) 2:", dcFun(dc1,1,2)
  print *, "dcFun(dc1) 3:", dcFun(dc1,1,3)
  print *, "dcFun(dc1) 4:", dcFun(dc1,1,4)

contains

  subroutine dcSub(arg,expect)
    type (dtChars(*)) :: arg
    integer, intent(in) :: expect

    print *, "dcSub: want:", expect, (expect+2), (2*expect), min(expect,2)
    print *, "dcSub:   is:", arg % l, len(arg%chPlus), len(arg%chMult), len(arg%chIntrin)
    print *, "dcSub:  arg:", arg
    call verifyLength("dcSub.l", arg%l, expect)
    call verifyLength("dcSub c", len(arg%ch), (expect))
    call verifyLength("dcSub +", len(arg%chPlus), (expect+2))
    call verifyLength("dcSub *", len(arg%chMult), (2*expect))
    call verifyLength("dcSub I", len(arg%chIntrin), min(expect,2))
  end subroutine dcSub

  function dcFun(arg,expect,i)
    type (dtChars(*)) :: arg
    integer, intent(in) :: expect, i
    character(max(expect+2,2*expect,min(expect,2))+2) :: dcFun

    if (arg%l /= expect) errorCount = errorCount + 1
    dcFun = repeat('.',len(dcFun))
    select case(i)
    case(1); dcFun(2:expect+1)   = arg%ch
    case(2); dcFun(2:expect+2+1) = arg%chPlus
    case(3); dcFun(2:2*expect+1) = arg%chMult
    case(4); dcFun(2:min(expect,2)+1) = arg%chIntrin
    end select
  end function dcFun

end subroutine extDCSub


subroutine extDIASub
  use :: dtpATPDADclExternalUseInternalMod
  implicit none
  type dtIntArray (l)
     integer, len :: l
     integer :: iarr(l)
     integer :: iarrMinus(l-2)
     integer :: iarrSquare(l**2)
     integer :: iarrIntrin(dim(4,l))
  end type dtIntArray

  type (dtIntArray(9)) :: dia9
  type (dtIntArray(2)) :: dia2
  integer :: i

  dia9 % iarr       = [(i+100,i=1,9)]
  dia9 % iarrMinus  = [(i,i=1,7)]
  dia9 % iarrSquare = [(81-i,i=1,81)]
  dia9 % iarrIntrin = [(i*2,i=1,0)]
  dia2 % iarr       = [(2**(i+20),i=1,2)]
  dia2 % iarrMinus  = [(i,i=1,0)]
  dia2 % iarrSquare = [(i**3,i=1,4)]
  dia2 % iarrIntrin = [(i*2,i=1,2)]

  call diaSub(dia9,9)
  call diaSub(dia2,2)

  print *, "diaFun(dia9) 1:", diaFun(dia9,9,1)
  print *, "diaFun(dia9) 2:", diaFun(dia9,9,2)
  print *, "diaFun(dia9) 3:", diaFun(dia9,9,3)
  print *, "diaFun(dia9) 4:", diaFun(dia9,9,4)

  print *, "diaFun(dia2) 1:", diaFun(dia2,2,1)
  print *, "diaFun(dia2) 2:", diaFun(dia2,2,2)
  print *, "diaFun(dia2) 3:", diaFun(dia2,2,3)
  print *, "diaFun(dia2) 4:", diaFun(dia2,2,4)

contains

  subroutine diaSub(arg,expect)
    type (dtIntArray(*)) :: arg
    integer, intent(in) :: expect

    print *, "diaSub: want:", expect, (expect-2), (expect**2), dim(4,expect)
    print *, "diaSub:   is:", arg % l, size(arg%iarrMinus), size(arg%iarrSquare), size(arg%iarrIntrin)
    print *, "diaSub:  arg:", arg
    call verifyLength("diaSub.l", arg%l, expect)
    call verifyLength("diaSub c", size(arg%iarr), expect)
    call verifyLength("diaSub -", size(arg%iarrMinus), (expect-2))
    call verifyLength("diaSub**", size(arg%iarrSquare), (expect**2))
    call verifyLength("diaSub I", size(arg%iarrIntrin), dim(4,expect))
  end subroutine diaSub

  function diaFun(arg,expect,i)
    type (dtIntArray(*)) :: arg
    integer, intent(in) :: expect, i
    integer :: diaFun(max(expect-2,expect**2,dim(4,expect))+2)

    if (arg%l /= expect) errorCount = errorCount + 1
    diaFun = -1
    select case(i)
    case(1); diaFun(2:size(arg%iarr)+1)       = arg%iarr
    case(2); diaFun(2:size(arg%iarrMinus)+1)  = arg%iarrMinus
    case(3); diaFun(2:size(arg%iarrSquare)+1) = arg%iarrSquare
    case(4); diaFun(2:size(arg%iarrIntrin)+1) = arg%iarrIntrin
    end select
  end function diaFun

end subroutine extDIASub