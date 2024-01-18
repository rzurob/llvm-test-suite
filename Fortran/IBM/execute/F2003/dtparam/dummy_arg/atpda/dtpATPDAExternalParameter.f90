!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-10-28
!*
!*  PRIMARY FUNCTIONS TESTED   : Assumed type parameters and dummy arguments
!*
!*  SECONDARY FUNCTIONS TESTED : external procedures applied to parameters (constants)
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
!*  Pass parameters (constant structures) in to procedures using assumed type parameters.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program dtpATPDAExternalParameter

  type dt(l)
     integer, len :: l
     sequence
  end type dt

  type dtChars (l)
     integer, len :: l
     sequence
     character(l)   :: ch
     character(l+2) :: chPlus
     character(2*l) :: chMult
     character(min(l,2)) :: chIntrin
  end type dtChars

  type dtIntArray (l)
     integer, len :: l
     sequence
     integer :: iarr(l)
     integer :: iarrMinus(l-2)
     integer :: iarrSquare(l**2)
     integer :: iarrIntrin(dim(4,l))
  end type dtIntArray


  type (dt(99)), parameter :: dt99 = dt(99)()
  type (dt(0)), parameter :: dt0 = dt(0)()
  type (dtChars(5)), parameter :: dc5 = dtChars(5)('aeiouy','zyxwvuts','abcdefghijk','pqr')
  type (dtChars(1)), parameter :: dc1 = dtChars(1)('qrs',   'lmno',    'kjih',       'gfe')
  type (dtIntArray(9)), parameter :: dia9 = dtIntArray(9)([(i+100,i=1,9)], [(i,i=1,7)], [(81-i,i=1,81)], [(i*2,i=1,0)])
  type (dtIntArray(2)), parameter :: dia2 = dtIntArray(2)([(2**(i+20),i=1,2)], [(i,i=1,0)], [(i**3,i=1,4)], [(i*2,i=1,2)])

  integer :: errorCount
  common /err/ errorCount

  interface
     subroutine sub1(arg,expect)
       import :: dt
       type (dt(*)) :: arg
       integer :: expect
     end subroutine sub1
     subroutine sub2(arg,expect)
       import :: dtChars
       type (dtChars(*)) :: arg
       integer :: expect
     end subroutine sub2
     subroutine sub3(arg,expect)
       import :: dtIntArray
       type (dtIntArray(*)) :: arg
       integer :: expect
     end subroutine sub3
  end interface

  errorCount = 0

  call sub1(dt99,99)
  call sub1(dt0,0)

  call sub2(dc5,5)
  call sub2(dc1,1)

  call sub3(dia9,9)
  call sub3(dia2,2)

  if (errorCount /= 0) print *, errorCount, "errors encountered."
  print *, "end"

end program dtpATPDAExternalParameter


subroutine verifyLength(name,have,expect)
  implicit none
  character(*) :: name
  integer :: have, expect
  integer :: errorCount
  common /err/ errorCount
  if (have /= expect) then
     print *, "  in ", name, ", arg%l is", have, "but should be", expect
     errorCount = errorCount + 1
  end if
end subroutine verifyLength


subroutine sub1(arg,expect)
  implicit none
  type dt(l)
     integer, len :: l
     sequence
  end type dt
  type (dt(*)) :: arg
  integer :: expect

  interface
     subroutine dtSub(arg,expect)
       import :: dt
       type (dt(*)) :: arg
       integer, intent(in) :: expect
     end subroutine dtSub
     integer function fun1(arg,expect)
       import :: dt
       type (dt(*)) :: arg
       integer :: expect
     end function fun1
  end interface

  call dtSub(arg, expect)
  print *, "fun1:", fun1(arg,expect)
end subroutine sub1


integer function fun1(arg,expect)
  implicit none
  type dt(l)
     integer, len :: l
     sequence
  end type dt
  type (dt(*)) :: arg
  integer, intent(in) :: expect
  interface
     integer function dtFun(arg,expect)
       import :: dt
       type (dt(*)) :: arg
       integer, intent(in) :: expect
     end function dtFun
  end interface

  fun1 = dtFun(arg, expect)
end function fun1


subroutine dtSub(arg,expect)
  implicit none
  type dt(l)
     integer, len :: l
     sequence
  end type dt
  type (dt(*)) :: arg
  integer, intent(in) :: expect
  interface
     subroutine verifyLength(name,have,expect)
       character(*) :: name
       integer :: have, expect
     end subroutine verifyLength
  end interface

  call verifyLength("dt.l", arg%l, expect)
end subroutine dtSub


integer function dtFun(arg,expect)
  implicit none
  type dt(l)
     integer, len :: l
     sequence
  end type dt
  type (dt(*)) :: arg
  integer, intent(in) :: expect

  dtFun = arg % l - expect
end function dtFun


subroutine sub2(arg,expect)
  implicit none
  type dtChars (l)
     integer, len :: l
     sequence
     character(l)   :: ch
     character(l+2) :: chPlus
     character(2*l) :: chMult
     character(min(l,2)) :: chIntrin
  end type dtChars
  type (dtChars(*)) :: arg
  integer :: expect

  interface
     subroutine dcSub(arg,expect)
       import :: dtChars
       type (dtChars(*)) :: arg
       integer, intent(in) :: expect
     end subroutine dcSub
     character(255) function fun2(arg,expect,i)
       import :: dtChars
       type (dtChars(*)) :: arg
       integer :: expect, i
     end function fun2
  end interface

  call dcSub(arg,expect)

  print *, "fun2 1:", trim(fun2(arg,expect,1))
  print *, "fun2 2:", trim(fun2(arg,expect,2))
  print *, "fun2 3:", trim(fun2(arg,expect,3))
  print *, "fun2 4:", trim(fun2(arg,expect,4))

end subroutine sub2


character(255) function fun2(arg,expect,i)
  implicit none
  type dtChars (l)
     integer, len :: l
     sequence
     character(l)   :: ch
     character(l+2) :: chPlus
     character(2*l) :: chMult
     character(min(l,2)) :: chIntrin
  end type dtChars
  type (dtChars(*)) :: arg
  integer :: expect, i
  interface
     function dcFun(arg,expect,i)
       import :: dtChars
       type (dtChars(*)) :: arg
       integer, intent(in) :: expect, i
       character(max(expect+2,2*expect,min(expect,2))+2) :: dcFun
     end function dcFun
  end interface

  fun2 = dcFun(arg,expect,i)
end function fun2


subroutine dcSub(arg,expect)
  implicit none
  type dtChars (l)
     integer, len :: l
     sequence
     character(l)   :: ch
     character(l+2) :: chPlus
     character(2*l) :: chMult
     character(min(l,2)) :: chIntrin
  end type dtChars
  type (dtChars(*)) :: arg
  integer, intent(in) :: expect
  interface
     subroutine verifyLength(name,have,expect)
       character(*) :: name
       integer :: have, expect
     end subroutine verifyLength
  end interface

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
  implicit none
  type dtChars (l)
     integer, len :: l
     sequence
     character(l)   :: ch
     character(l+2) :: chPlus
     character(2*l) :: chMult
     character(min(l,2)) :: chIntrin
  end type dtChars
  type (dtChars(*)) :: arg
  integer, intent(in) :: expect, i
  character(max(expect+2,2*expect,min(expect,2))+2) :: dcFun
  integer :: errorCount
  common /err/     errorCount

  if (arg%l /= expect) errorCount = errorCount + 1
  dcFun = repeat('.',len(dcFun))
  select case(i)
  case(1); dcFun(2:expect+1)   = arg%ch
  case(2); dcFun(2:expect+2+1) = arg%chPlus
  case(3); dcFun(2:2*expect+1) = arg%chMult
  case(4); dcFun(2:min(expect,2)+1) = arg%chIntrin
  end select
end function dcFun




subroutine sub3(arg,expect)
  implicit none
  type dtIntArray (l)
     integer, len :: l
     sequence
     integer :: iarr(l)
     integer :: iarrMinus(l-2)
     integer :: iarrSquare(l**2)
     integer :: iarrIntrin(dim(4,l))
  end type dtIntArray
  type (dtIntArray(*)) :: arg
  integer :: expect

  interface
     subroutine diaSub(arg,expect)
       import :: dtIntArray
       type (dtIntArray(*)) :: arg
       integer, intent(in) :: expect
     end subroutine diaSub
     function fun3(arg,expect,i)
       import :: dtIntArray
       type (dtIntArray(*)) :: arg
       integer, intent(in) :: expect, i
       integer :: fun3(100)
     end function fun3
  end interface

  call diaSub(arg,expect)

  print *, "fun3 1:", fun3(arg,expect,1)
  print *, "fun3 2:", fun3(arg,expect,2)
  print *, "fun3 3:", fun3(arg,expect,3)
  print *, "fun3 4:", fun3(arg,expect,4)

end subroutine sub3


integer function fun3(arg,expect,i)
  type dtIntArray (l)
     integer, len :: l
     sequence
     integer :: iarr(l)
     integer :: iarrMinus(l-2)
     integer :: iarrSquare(l**2)
     integer :: iarrIntrin(dim(4,l))
  end type dtIntArray
  type (dtIntArray(*)) :: arg
  integer :: expect, i
  dimension :: fun3(100)
  interface
     function diaFun(arg,expect,i)
       import :: dtIntArray
       type (dtIntArray(*)) :: arg
       integer, intent(in) :: expect, i
       integer :: diaFun(max(expect-2,expect**2,dim(4,expect))+2)
     end function diaFun
  end interface

  fun3 = reshape(diaFun(arg,expect,i),shape=[100],pad=[-1])
end function fun3


subroutine diaSub(arg,expect)
  implicit none
  type dtIntArray (l)
     integer, len :: l
     sequence
     integer :: iarr(l)
     integer :: iarrMinus(l-2)
     integer :: iarrSquare(l**2)
     integer :: iarrIntrin(dim(4,l))
  end type dtIntArray
  type (dtIntArray(*)) :: arg
  integer, intent(in) :: expect
  interface
     subroutine verifyLength(name,have,expect)
       character(*) :: name
       integer :: have, expect
     end subroutine verifyLength
  end interface

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
  implicit none
  type dtIntArray (l)
     integer, len :: l
     sequence
     integer :: iarr(l)
     integer :: iarrMinus(l-2)
     integer :: iarrSquare(l**2)
     integer :: iarrIntrin(dim(4,l))
  end type dtIntArray
  type (dtIntArray(*)) :: arg
  integer, intent(in) :: expect, i
  integer :: diaFun(max(expect-2,expect**2,dim(4,expect))+2)
  integer :: errorCount
  common /err/     errorCount

  if (arg%l /= expect) errorCount = errorCount + 1
  diaFun = -1
  select case(i)
  case(1); diaFun(2:size(arg%iarr)+1)       = arg%iarr
  case(2); diaFun(2:size(arg%iarrMinus)+1)  = arg%iarrMinus
  case(3); diaFun(2:size(arg%iarrSquare)+1) = arg%iarrSquare
  case(4); diaFun(2:size(arg%iarrIntrin)+1) = arg%iarrIntrin
  end select
end function diaFun
