!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-05-22
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : allocate DTP array (len parameter) with allocatable component via intrinsic assignment
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpIAABasic005 (<-dtpIAABasic002<-dtpIAABasic001)
!*
!*  DESCRIPTION
!*
!*  The simplest case of an allocatable DTP array with a length parameter, we verify
!*  that the variables are initially unallocated, allocated after assignment, and deallocated
!*  before assignment. We can only verify the latter indirectly, by printing a message when
!*  something is finalized, which only happens here after structure constructors in exprs
!*  have been referenced, and after allocated instances of derived types are deallocated.
!*  There are two important caveats about using finalization in this context:
!*  1. Where multiple objects may be finalized, the precise order of finalization
!*     may be processor dependent.  The program below has been constructed so as to
!*     avoid this situation.
!*  2. The entities below are in the main program, and are not finalized immediately
!*     before the END statement.  If the code were executed instead in a subroutine,
!*     additional finalizations would be performed (and the order in which each entity
!*     is finalized would not be guaranteed).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAAArray005mod

  implicit none
  type dla(l)
     integer, len :: l
     character(l) :: label = ''
     integer, allocatable :: iarr(:)
   contains
     final :: fin, finArray
  end type dla

  integer, save :: step = 0

contains

  subroutine fin(a)
    type(dla(*)) :: a
    if (allocated(a%iarr)) then
       print *, step, "fin:", a%l, len(a%label), size(a%iarr), ">", a%label, "<", a%iarr
       deallocate(a%iarr)
    else
       print *, step, "fin:", a%l, len(a%label), 0, ">", a%label, "<, iarr not allocated"
    end if
  end subroutine fin

  subroutine finArray(a)
    type(dla(*)) :: a(:)
    print *, step, "finArray:", a%l, len(a%label), size(a)
  end subroutine finArray

end module dtpIAAArray005mod



program dtpIAAArray005

  use dtpIAAArray005mod
  implicit none

  type(dla(:)), allocatable :: v1(:), v2(:), v3(:)
  type(dla(4)) :: v4
  integer :: i

  step = 10
  print *, step, allocated(v1), allocated(v2), allocated(v3)

  step = 11
  print *, step, "v1 = [dla(1)('a',[1234321])]"
  step = 12
  v1 = [dla(1)('a',[1234321])]

  step = 13
  print *, step, "v2 = v1 {v1=", allocated(v1), (allocated(v1(i)%iarr), v1(i)%label, v1(i)%iarr, i=1,size(v1)), "}"
  step = 14
  v2 = v1

  v2(1)%label = 'b'
  v2(1)%iarr  = [2345432]
  v1(1)%label = 'c'
  v1(1)%iarr  = [3456543]

  step = 15
  print *, step, "v1 = v2 {v1=", allocated(v1), (allocated(v1(i)%iarr), v1(i)%label, v1(i)%iarr, i=1,size(v1)), ", v2=", allocated(v2), (allocated(v2(i)%iarr), v2(i)%label, v2(i)%iarr, i=1,size(v2)), "}"
  step = 16
  v1 = v2

  step = 17
  print *, step, "v3 = [dla(3)('def',[44444,55555,66666]),dla(3)('jkl',[3333,2222,1111])]"
  step = 18
  v3 = [dla(3)('def',[44444,55555,66666]),dla(3)('jkl',[3333,2222,1111])]

  step = 19
  print *, step, "v2 = v3 {v2=", allocated(v2), (allocated(v2(i)%iarr), v2(i)%label, v2(i)%iarr, i=1,size(v2)), ", v3=", allocated(v3), (allocated(v3(i)%iarr), v3(i)%label, v3(i)%iarr, i=1,size(v3)), "}"
  step = 20
  v2 = v3

  step = 21
  print *, step, "v3 = v1 {v3=", allocated(v3), (allocated(v3(i)%iarr), v3(i)%label, v3(i)%iarr, i=1,size(v3)), ", v1=", allocated(v1), (allocated(v1(i)%iarr), v1(i)%label, v1(i)%iarr, i=1,size(v1)), "}"
  step = 22
  v3 = v1


  step = 23
  print *, step, "v4 = dla(4)('ghij',[777,888,999,1000])"
  step = 24
  v4 = dla(4)('ghij',[777,888,999,1000])

  step = 25
  print *, step, "v1 = v4 {v1=", allocated(v1), (allocated(v1(i)%iarr), v1(i)%label, v1(i)%iarr, i=1,size(v1)), ", v4=", allocated(v4%iarr), v4%label, v4%iarr, "}"
  step = 26
  v1 = v4

  print *, 91, allocated(v1), v1%l, (allocated(v1(i)%iarr), v1(i)%label, size(v1(i)%iarr), v1(i)%iarr, i=1,size(v1))
  print *, 92, allocated(v2), v2%l, (allocated(v2(i)%iarr), v2(i)%label, size(v2(i)%iarr), v2(i)%iarr, i=1,size(v2))
  print *, 93, allocated(v3), v3%l, (allocated(v3(i)%iarr), v3(i)%label, size(v3(i)%iarr), v3(i)%iarr, i=1,size(v3))

end program dtpIAAArray005
