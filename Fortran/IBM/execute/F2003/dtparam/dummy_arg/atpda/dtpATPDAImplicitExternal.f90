!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpATPDAImplicitExternal
!*
!*  DATE                       : 2008-10-15
!*
!*  PRIMARY FUNCTIONS TESTED   : Assumed type parameters and dummy arguments
!*
!*  SECONDARY FUNCTIONS TESTED : implicit declaration of sequence DTP and especially assumed type parameter variables
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
!*  Declare types in external procedures and use implicit statements to declare
!*  objects and assumed type parameter dummy arguments.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


program dtpATPDAImplicitExternal

  implicit none
  integer :: ierrorCount
  common /err/     ierrorCount

  call sub1
  call sub2
  call sub3

  if (ierrorCount /= 0) print *, ierrorCount, "errors encountered."
  print *, "end"

end program dtpATPDAImplicitExternal


subroutine verifyLength(cname,ihave,iexpect)
  implicit character(*)(c)
  intent(in) :: ihave, iexpect
  common /err/ ierrorCount
  if (ihave /= iexpect) then
     print *, "  in ", cname, ", arg%l is", ihave, "but should be", iexpect
     ierrorCount = ierrorCount + 1
  end if
end subroutine verifyLength


block data dtpATPDAImplicitExternalDataBlock

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

  implicit type(dt(99))(u), type(dt(0))(v), type(dtChars(5))(w), type(dtChars(1))(x), type(dtIntArray(9))(y), type(dtIntArray(2))(z)
  implicit type(dt(*))(q), type(dtChars(*))(r), type(dtIntArray(*))(s)

  integer :: ierrorCount

  common /err/ ierrorCount
  common /dttest/ udt99, vdt0
  common /dctest/ wdc5, xdc1
  common /diatest/ ydia9, zdia2

  data wdc5 /dtChars(5)('aeiouy','zyxwvuts','abcdefghijk','pqr')/
  data xdc1 /dtChars(1)('qrs',   'lmno',    'kjih',       'gfe')/
  data ydia9/dtIntArray(9)([(i+100,i=1,9)], [(i,i=1,7)], [(81-i,i=1,81)], [(i*2,i=1,0)])/
  data zdia2/dtIntArray(2)([(2**(i+20),i=1,2)], [(i,i=1,0)], [(i**3,i=1,4)], [(i*2,i=1,2)])/
  data ierrorCount /0/

end block data dtpATPDAImplicitExternalDataBlock


subroutine sub1
  type dt(l)
     integer, len :: l
     sequence
  end type dt
  implicit type(dt(99))(u), type(dt(0))(v), type(dt(*))(q)
  common /dttest/  udt99, vdt0
  interface
     subroutine dtSub(qarg,iexpect)
       import :: dt
       implicit type(dt(99))(u), type(dt(0))(v), type(dt(*))(q)
     end subroutine dtSub
     integer function dtFun(qarg,iexpect)
       import :: dt
       implicit type(dt(99))(u), type(dt(0))(v), type(dt(*))(q)
     end function dtFun
  end interface
  call dtSub(udt99,99)
  call dtSub(vdt0,0)
  print *, "dtFun(udt99):", dtFun(udt99,99)
  print *, "dtFun(vdt0):", dtFun(vdt0,0)
end subroutine sub1

subroutine sub2
  type dtChars (l)
     integer, len :: l
     sequence
     character(l)   :: ch
     character(l+2) :: chPlus
     character(2*l) :: chMult
     character(min(l,2)) :: chIntrin
  end type dtChars
  implicit type(dtChars(5))(w), type(dtChars(1))(x), type(dtChars(*))(r)
  common /dctest/  wdc5, xdc1
  interface
     subroutine dcSub(rarg,iexpect)
       import :: dtChars
       implicit type(dtChars(5))(w), type(dtChars(1))(x), type(dtChars(*))(r)
     end subroutine dcSub
     function dcFun(rarg,iexpect,i)
       import :: dtChars
       implicit type(dtChars(5))(w), type(dtChars(1))(x), type(dtChars(*))(r)
       character(max(iexpect+2,2*iexpect,min(iexpect,2))+2) :: dcFun
     end function dcFun
  end interface
  call dcSub(wdc5,5)
  call dcSub(xdc1,1)

  print *, "dcFun(wdc5) 1:", dcFun(wdc5,5,1)
  print *, "dcFun(wdc5) 2:", dcFun(wdc5,5,2)
  print *, "dcFun(wdc5) 3:", dcFun(wdc5,5,3)
  print *, "dcFun(wdc5) 4:", dcFun(wdc5,5,4)

  print *, "dcFun(xdc1) 1:", dcFun(xdc1,1,1)
  print *, "dcFun(xdc1) 2:", dcFun(xdc1,1,2)
  print *, "dcFun(xdc1) 3:", dcFun(xdc1,1,3)
  print *, "dcFun(xdc1) 4:", dcFun(xdc1,1,4)

end subroutine sub2

subroutine sub3
  type dtIntArray (l)
     integer, len :: l
     sequence
     integer :: iarr(l)
     integer :: iarrMinus(l-2)
     integer :: iarrSquare(l**2)
     integer :: iarrIntrin(dim(4,l))
  end type dtIntArray
  implicit type(dtIntArray(9))(y), type(dtIntArray(2))(z), type(dtIntArray(*))(s)
  common /diatest/ ydia9, zdia2
  interface
     subroutine diaSub(sarg,iexpect)
       import :: dtIntArray
       implicit type(dtIntArray(9))(y), type(dtIntArray(2))(z), type(dtIntArray(*))(s)
     end subroutine diaSub
     function diaFun(sarg,iexpect,i)
       import :: dtIntArray
       implicit type(dtIntArray(9))(y), type(dtIntArray(2))(z), type(dtIntArray(*))(s)
       integer :: diaFun(max(iexpect-2,iexpect**2,dim(4,iexpect))+2)
     end function diaFun
  end interface
  call diaSub(ydia9,9)
  call diaSub(zdia2,2)

  print *, "diaFun(ydia9) 1:", diaFun(ydia9,9,1)
  print *, "diaFun(ydia9) 2:", diaFun(ydia9,9,2)
  print *, "diaFun(ydia9) 3:", diaFun(ydia9,9,3)
  print *, "diaFun(ydia9) 4:", diaFun(ydia9,9,4)

  print *, "diaFun(zdia2) 1:", diaFun(zdia2,2,1)
  print *, "diaFun(zdia2) 2:", diaFun(zdia2,2,2)
  print *, "diaFun(zdia2) 3:", diaFun(zdia2,2,3)
  print *, "diaFun(zdia2) 4:", diaFun(zdia2,2,4)
end subroutine sub3


subroutine dtSub(qarg,iexpect)
  type dt(l)
     integer, len :: l
     sequence
  end type dt
  implicit type(dt(99))(u), type(dt(0))(v), type(dt(*))(q), character(*)(c)
  interface
     subroutine verifyLength(cname,ihave,iexpect)
       implicit character(*)(c)
     end subroutine verifyLength
  end interface
  intent(in) :: iexpect

  call verifyLength("dt.l", qarg%l, iexpect)
end subroutine dtSub


subroutine dcSub(rarg,iexpect)
  type dtChars (l)
     integer, len :: l
     sequence
     character(l)   :: ch
     character(l+2) :: chPlus
     character(2*l) :: chMult
     character(min(l,2)) :: chIntrin
  end type dtChars
  implicit type(dtChars(5))(w), type(dtChars(1))(x), type(dtChars(*))(r), character(*)(c)
  intent(in) :: iexpect
  interface
     subroutine verifyLength(cname,ihave,iexpect)
       implicit character(*)(c)
     end subroutine verifyLength
  end interface

  print *, "dcSub: want:", iexpect, (iexpect+2), (2*iexpect), min(iexpect,2)
  print *, "dcSub:   is:", rarg % l, len(rarg%chPlus), len(rarg%chMult), len(rarg%chIntrin)
  print *, "dcSub:  rarg:", rarg
  call verifyLength("dcSub.l", rarg%l, iexpect)
  call verifyLength("dcSub c", len(rarg%ch), (iexpect))
  call verifyLength("dcSub +", len(rarg%chPlus), (iexpect+2))
  call verifyLength("dcSub *", len(rarg%chMult), (2*iexpect))
  call verifyLength("dcSub I", len(rarg%chIntrin), min(iexpect,2))
end subroutine dcSub


subroutine diaSub(sarg,iexpect)
  type dtIntArray (l)
     integer, len :: l
     sequence
     integer :: iarr(l)
     integer :: iarrMinus(l-2)
     integer :: iarrSquare(l**2)
     integer :: iarrIntrin(dim(4,l))
  end type dtIntArray
  implicit type(dtIntArray(9))(y), type(dtIntArray(2))(z), type(dtIntArray(*))(s), character(*)(c)
  intent(in) :: iexpect
  interface
     subroutine verifyLength(cname,ihave,iexpect)
       implicit character(*)(c)
     end subroutine verifyLength
  end interface

  print *, "diaSub: want:", iexpect, (iexpect-2), (iexpect**2), dim(4,iexpect)
  print *, "diaSub:   is:", sarg % l, size(sarg%iarrMinus), size(sarg%iarrSquare), size(sarg%iarrIntrin)
  print *, "diaSub:  arg:", sarg
  call verifyLength("diaSub.l", sarg%l, iexpect)
  call verifyLength("diaSub c", size(sarg%iarr), iexpect)
  call verifyLength("diaSub -", size(sarg%iarrMinus), (iexpect-2))
  call verifyLength("diaSub**", size(sarg%iarrSquare), (iexpect**2))
  call verifyLength("diaSub I", size(sarg%iarrIntrin), dim(4,iexpect))
end subroutine diaSub


integer function dtFun(qarg,iexpect)
  type dt(l)
     integer, len :: l
     sequence
  end type dt
  implicit type(dt(99))(u), type(dt(0))(v), type(dt(*))(q)
  intent(in) :: iexpect
  common /err/     ierrorCount

  if (qarg%l /= iexpect) ierrorCount = ierrorCount + 1
  dtFun = qarg % l - iexpect
end function dtFun

function dcFun(rarg,iexpect,i)
  type dtChars (l)
     integer, len :: l
     sequence
     character(l)   :: ch
     character(l+2) :: chPlus
     character(2*l) :: chMult
     character(min(l,2)) :: chIntrin
  end type dtChars
  implicit type(dtChars(5))(w), type(dtChars(1))(x), type(dtChars(*))(r)
  intent(in) :: iexpect
  common /err/     ierrorCount
  character(max(iexpect+2,2*iexpect,min(iexpect,2))+2) :: dcFun

  if (rarg%l /= iexpect) ierrorCount = ierrorCount + 1
  dcFun = repeat('.',len(dcFun))
  select case(i)
  case(1); dcFun(2:iexpect+1)   = rarg%ch
  case(2); dcFun(2:iexpect+2+1) = rarg%chPlus
  case(3); dcFun(2:2*iexpect+1) = rarg%chMult
  case(4); dcFun(2:min(iexpect,2)+1) = rarg%chIntrin
  end select
end function dcFun

function diaFun(sarg,iexpect,i)
  type dtIntArray (l)
     integer, len :: l
     sequence
     integer :: iarr(l)
     integer :: iarrMinus(l-2)
     integer :: iarrSquare(l**2)
     integer :: iarrIntrin(dim(4,l))
  end type dtIntArray
  implicit type(dtIntArray(9))(y), type(dtIntArray(2))(z), type(dtIntArray(*))(s)
  intent(in) :: iexpect
  common /err/     ierrorCount
  integer :: diaFun(max(iexpect-2,iexpect**2,dim(4,iexpect))+2)

  if (sarg%l /= iexpect) ierrorCount = ierrorCount + 1
  diaFun = -1
  select case(i)
  case(1); diaFun(2:size(sarg%iarr)+1)       = sarg%iarr
  case(2); diaFun(2:size(sarg%iarrMinus)+1)  = sarg%iarrMinus
  case(3); diaFun(2:size(sarg%iarrSquare)+1) = sarg%iarrSquare
  case(4); diaFun(2:size(sarg%iarrIntrin)+1) = sarg%iarrIntrin
  end select
end function diaFun
