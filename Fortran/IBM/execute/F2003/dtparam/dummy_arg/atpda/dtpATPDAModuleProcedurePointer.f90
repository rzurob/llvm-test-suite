!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpATPDAModuleProcedurePointer
!*
!*  DATE                       : 2008-10-15
!*
!*  PRIMARY FUNCTIONS TESTED   : Assumed type parameters and dummy arguments
!*
!*  SECONDARY FUNCTIONS TESTED : procedure pointers in a module
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
!*  Invoke module procedures on types via pointers.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpATPDAModuleProcedurePointerMod

  implicit none

  type dt(l)
     integer, len :: l
  end type dt

  type dtChars (l)
     integer, len :: l
     character(l)   :: ch
     character(l+2) :: chPlus
     character(2*l) :: chMult
     character(min(l,2)) :: chIntrin
  end type dtChars

  type dtIntArray (l)
     integer, len :: l
     integer :: iarr(l)
     integer :: iarrMinus(l-2)
     integer :: iarrSquare(l**2)
     integer :: iarrIntrin(dim(4,l))
  end type dtIntArray

  integer :: errorCount, i

  type (dt(99))     :: dt99
  type (dt(0))      :: dt0
  type (dtChars(5)) :: dc5
  type (dtChars(1)) :: dc1
  type (dtIntArray(9)) :: dia9
  type (dtIntArray(2)) :: dia2

contains

  subroutine verifyLength(name,have,expect)
    character(*) :: name
    integer :: have, expect
    integer :: errorCount
    if (have /= expect) then
       print *, "  in ", name, ", arg%l is", have, "but should be", expect
       errorCount = errorCount + 1
    end if
  end subroutine verifyLength


  subroutine sub1(p1,p2,o,expect)
    type (dt(*)) :: o
    integer, intent(in) :: expect
    interface
       subroutine p1(o,expect)
         import :: dt
         type (dt(*)) :: o
         integer, intent(in) :: expect
       end subroutine p1
       integer function p2(o,expect)
         import :: dt
         type (dt(*)) :: o
         integer, intent(in) :: expect
       end function p2
    end interface

    call p1(o, expect)
    print *, "p2:", expect, p2(o,expect)

  end subroutine sub1

  subroutine sub2(p1, p2, o, expect)
    type (dtChars(*)) :: o
    integer, intent(in) :: expect
    interface
       subroutine p1(arg,expect)
         import :: dtChars
         type (dtChars(*)) :: arg
         integer, intent(in) :: expect
       end subroutine p1
       function p2(arg,expect,i)
         import :: dtChars
         type (dtChars(*)) :: arg
         integer, intent(in) :: expect, i
         character(max(expect+2,2*expect,min(expect,2))+2) :: p2
       end function p2
    end interface

    call p1(o,expect)
    print *, "p2 1:", expect, p2(o,expect,1)
    print *, "p2 2:", expect, p2(o,expect,2)
    print *, "p2 3:", expect, p2(o,expect,3)
    print *, "p2 4:", expect, p2(o,expect,4)
  end subroutine sub2

  subroutine sub3(p1, p2, o, expect)
    type (dtIntArray(*)) :: o
    integer, intent(in) :: expect
    interface
       subroutine p1(arg,expect)
         import :: dtIntArray
         type (dtIntArray(*)) :: arg
         integer, intent(in) :: expect
       end subroutine p1
       function p2(arg,expect,i)
         import :: dtIntArray
         type (dtIntArray(*)) :: arg
         integer, intent(in) :: expect, i
         integer :: p2(max(expect-2,expect**2,dim(4,expect))+2)
       end function p2
    end interface

    call p1(o,expect)

    print *, "p2 1:", expect, p2(o,expect,1)
    print *, "p2 2:", expect, p2(o,expect,2)
    print *, "p2 3:", expect, p2(o,expect,3)
    print *, "p2 4:", expect, p2(o,expect,4)

  end subroutine sub3



  subroutine dtSub(arg,expect)
    type (dt(*)) :: arg
    integer, intent(in) :: expect
    call verifyLength("dt.l", arg%l, expect)
  end subroutine dtSub

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


  integer function dtFun(arg,expect)
    type (dt(*)) :: arg
    integer, intent(in) :: expect

    dtFun = arg % l - expect
  end function dtFun

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

end module dtpATPDAModuleProcedurePointerMod




program dtpATPDAModuleProcedurePointer

  use :: dtpATPDAModuleProcedurePointerMod

  dt99 = dt(99)()
  dt0  = dt(0)()
  dc5  = dtChars(5)('aeiouy','zyxwvuts','abcdefghijk','pqr')
  dc1  = dtChars(1)('qrs',   'lmno',    'kjih',       'gfe')
  dia9 = dtIntArray(9)([(i+100,i=1,9)], [(i,i=1,7)], [(81-i,i=1,81)], [(i*2,i=1,0)])
  dia2 = dtIntArray(2)([(2**(i+20),i=1,2)], [(i,i=1,0)], [(i**3,i=1,4)], [(i*2,i=1,2)])
  errorCount = 0

  call sub1(dtSub, dtFun, dt99, 99)
  call sub1(dtSub, dtFun, dt0, 0)

  call sub2(dcSub, dcFun, dc5, 5)
  call sub2(dcSub, dcFun, dc1, 1)

  call sub3(diaSub, diaFun, dia9, 9)
  call sub3(diaSub, diaFun, dia2, 2)

  if (errorCount /= 0) print *, errorCount, "errors encountered."
  print *, "end"

end program dtpATPDAModuleProcedurePointer
