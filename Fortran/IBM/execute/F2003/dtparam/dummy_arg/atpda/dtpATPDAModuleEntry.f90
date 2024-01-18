!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpATPDAModuleEntry
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-10-28
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Assumed type parameters and dummy arguments
!*
!*  SECONDARY FUNCTIONS TESTED : module ENTRY procedures
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
!*  Pass structures to module procedures declared with ENTRY statements.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpATPDAModuleEntryMod

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

  type (dt(99))     :: dt99
  type (dt(0))      :: dt0
  type (dtChars(5)) :: dc5
  type (dtChars(1)) :: dc1
  type (dtIntArray(9)) :: dia9
  type (dtIntArray(2)) :: dia2

  integer :: errorCount

contains

  subroutine verifyLength(name,have,expect)
    implicit none
    character(*) :: name
    integer :: have, expect
    if (have /= expect) then
       print *, "  in ", name, ", arg%l is", have, "but should be", expect
       errorCount = errorCount + 1
    end if
  end subroutine verifyLength


  subroutine sub1(qarg,expect)
    implicit none
    type (dt(*)) :: qarg
    type (dtChars(*)) :: rarg
    type (dtIntArray(*)) :: sarg
    integer :: expect

    call dtSub(qarg, expect)
    print *, "fun1:", fun1(qarg,expect)
    return

  entry sub2(rarg,expect)
    call dcSub(rarg,expect)

    print *, "fun2 1:", trim(fun2(rarg,expect,1))
    print *, "fun2 2:", trim(fun2(rarg,expect,2))
    print *, "fun2 3:", trim(fun2(rarg,expect,3))
    print *, "fun2 4:", trim(fun2(rarg,expect,4))
    return

  entry sub3(sarg,expect)

    call diaSub(sarg,expect)

    print *, "fun3 1:", fun3(sarg,expect,1)
    print *, "fun3 2:", fun3(sarg,expect,2)
    print *, "fun3 3:", fun3(sarg,expect,3)
    print *, "fun3 4:", fun3(sarg,expect,4)

  end subroutine sub1


  subroutine dtSub(qarg,expect)
    implicit none

    type (dt(*)) :: qarg
    type (dtChars(*)) :: rarg
    type (dtIntArray(*)) :: sarg
    integer, intent(in) :: expect

    call verifyLength("dt.l", qarg%l, expect)
    return

  entry dcSub(rarg,expect)
    print *, "dcSub: want:", expect, (expect+2), (2*expect), min(expect,2)
    print *, "dcSub:   is:", rarg % l, len(rarg%chPlus), len(rarg%chMult), len(rarg%chIntrin)
    print *, "dcSub:  rarg:", rarg
    call verifyLength("dcSub.l", rarg%l, expect)
    call verifyLength("dcSub c", len(rarg%ch), (expect))
    call verifyLength("dcSub +", len(rarg%chPlus), (expect+2))
    call verifyLength("dcSub *", len(rarg%chMult), (2*expect))
    call verifyLength("dcSub I", len(rarg%chIntrin), min(expect,2))
    return

  entry diaSub(sarg,expect)
    print *, "diaSub: want:", expect, (expect-2), (expect**2), dim(4,expect)
    print *, "diaSub:   is:", sarg % l, size(sarg%iarrMinus), size(sarg%iarrSquare), size(sarg%iarrIntrin)
    print *, "diaSub:  sarg:", sarg
    call verifyLength("diaSub.l", sarg%l, expect)
    call verifyLength("diaSub c", size(sarg%iarr), expect)
    call verifyLength("diaSub -", size(sarg%iarrMinus), (expect-2))
    call verifyLength("diaSub**", size(sarg%iarrSquare), (expect**2))
    call verifyLength("diaSub I", size(sarg%iarrIntrin), dim(4,expect))

  end subroutine dtSub


  integer function fun1Raw(expect)
    implicit none
    type (dt(*)) :: arg
    integer :: fun1
    integer, intent(in) :: expect

    fun1 = dtFun(dt(expect)(), expect)
    return

  entry fun1(arg,expect)
    fun1 = dtFun(arg, expect)
  end function fun1Raw


  character(255) function fun2Raw(expect,i)
    implicit none
    character(255) :: fun2
    type (dtChars(*)) :: arg
    integer :: expect, i

    fun2 = dcFun(dtChars(expect)('aaaaaaaaaaaaaaaaa','bbbbbbbbbbbbbbbbb','ccccccccccccccccc','ddddddddddddddddd'),expect,i)
    return

  entry fun2(arg,expect,i)
    fun2 = dcFun(arg,expect,i)

  end function fun2Raw


  integer function fun3Raw(expect,i)

    type (dtIntArray(*)) :: arg
    integer :: expect, i
    dimension :: fun3Raw(100)
    integer :: fun3(100)

    fun3 = reshape(diaFun(dtIntArray(expect)(11,22,33,44),expect,i),shape=[100],pad=[-1])
    return

  entry fun3(arg,expect,i)
    fun3 = reshape(diaFun(arg,expect,i),shape=[100],pad=[-1])
  end function fun3Raw


  integer function dtFun(arg,expect)
    implicit none
    type (dt(*)) :: arg
    integer, intent(in) :: expect

    dtFun = arg % l - expect
  end function dtFun



  function dcFun(arg,expect,i)
    implicit none
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
    implicit none
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

end module dtpATPDAModuleEntryMod

program dtpATPDAModuleEntry

  use :: dtpATPDAModuleEntryMod

  dc5  = dtChars(5)('aeiouy','zyxwvuts','abcdefghijk','pqr')
  dc1  = dtChars(1)('qrs',   'lmno',    'kjih',       'gfe')
  dia9 = dtIntArray(9)([(i+100,i=1,9)], [(i,i=1,7)], [(81-i,i=1,81)], [(i*2,i=1,0)])
  dia2 = dtIntArray(2)([(2**(i+20),i=1,2)], [(i,i=1,0)], [(i**3,i=1,4)], [(i*2,i=1,2)])
  errorCount = 0

  call sub1(dt99,99)
  call sub1(dt0,0)

  call sub2(dc5,5)
  call sub2(dc1,1)

  call sub3(dia9,9)
  call sub3(dia2,2)

  ! Just to be on the safe side:
  print *, "fun1Raw:", fun1Raw(1)
  print *, "fun2Raw:", fun2Raw(2,1), fun2Raw(2,2), fun2Raw(2,3), fun2Raw(2,4)
  print *, "fun3Raw:", fun3Raw(3,1), fun3Raw(3,2), fun3Raw(3,3), fun3Raw(3,4)

  if (errorCount /= 0) print *, errorCount, "errors encountered."
  print *, "end"

end program dtpATPDAModuleEntry
