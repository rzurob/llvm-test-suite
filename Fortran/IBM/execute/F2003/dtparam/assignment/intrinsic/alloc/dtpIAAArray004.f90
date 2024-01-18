!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpIAAArray004
!*
!*  DATE                       : 2009-05-22
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : allocate DTP array (kind parameter) with allocatable component via intrinsic assignment
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpIAABasic004 ()
!*
!*  DESCRIPTION
!*
!*  The simplest case of an allocatable DTP array with a kind parameter, with allocatable
!*  components, we verify that the variables are initially unallocated, allocated after assignment,
!*  and deallocated before assignment. We can only verify the latter indirectly, by printing a
!*  message when something is finalized, which only happens here after structure constructors in
!*  exprs have been referenced, and after allocated instances of derived types are deallocated.
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

module dtpIAAArray004mod

  implicit none
  type dka(k)
     integer, kind :: k
     integer(k), allocatable :: ivar
   contains
     final :: f1, f1arr, f4, f4arr
  end type dka

  integer, save :: step = 0

contains

  subroutine f1(a)
    type(dka(1)) :: a
    if (allocated(a%ivar)) then
       print *, step, "f1:", a%k, kind(a%ivar), a%ivar
       deallocate(a%ivar)
    else
       print *, step, "f1:", a%k, kind(a%ivar), "not allocated"
    end if
  end subroutine f1

  subroutine f1arr(a)
    type(dka(1)) :: a(:)
    print *, step, "f1arr:", a%k, size(a)
  end subroutine f1arr

  subroutine f4(a)
    type(dka(4)) :: a
    if (allocated(a%ivar)) then
       print *, step, "f4:", a%k, kind(a%ivar), a%ivar
       deallocate(a%ivar)
    else
       print *, step, "f4:", a%k, kind(a%ivar), "not allocated"
    end if
  end subroutine f4

  subroutine f4arr(a)
    type(dka(4)) :: a(:)
    print *, step, "f4arr:", a%k, size(a)
  end subroutine f4arr

end module dtpIAAArray004mod



program dtpIAAArray004

  use dtpIAAArray004mod
  implicit none

  type(dka(1)), allocatable :: v1(:), v1a(:), v1b(:)
  type(dka(1)) :: v1c
  type(dka(4)), allocatable :: v4(:), v4a(:), v4b(:)
  type(dka(4)) :: v4c
  integer :: i


  step = 10
  print *, step, allocated(v1), allocated(v1a), allocated(v1b)

  step = 11
  print *, step, "v1 = [dka(1)(101)]"

  step = 12
  v1 = [dka(1)(101)]

  step = 13
  print *, step, "v1a = v1 {v1=", allocated(v1), (allocated(v1(i)%ivar), v1(i)%ivar,i=1,size(v1)), "}"

  step = 14
  v1a = v1
  v1a(1)%ivar = 99
  v1(1)%ivar = 98

  step = 15
  print *, step, "v1 = v1a {v1=", allocated(v1), (allocated(v1(i)%ivar),v1(i)%ivar,i=1,size(v1)), ", v1a=", allocated(v1a), (allocated(v1a(i)%ivar),v1a(i)%ivar,i=1,size(v1a)), "}"

  step = 16
  v1 = v1a

  step = 17
  print *, step, "v1b = [dka(1)(102), dka(1)(103), dka(1)(104), dka(1)(105)]"

  step = 18
  v1b = [dka(1)(102), dka(1)(103), dka(1)(104), dka(1)(105)]

  step = 19
  print *, step, "v1a = v1b {v1a=", allocated(v1a), (allocated(v1a(i)%ivar),v1a(i)%ivar,i=1,size(v1a)), ", v1b=", allocated(v1b), (allocated(v1b(i)%ivar),v1b(i)%ivar,i=1,size(v1b)), "}"

  step = 20
  v1a = v1b

  step = 21
  print *, step, "v1c = dka(1)(106)"

  step = 22
  v1c = dka(1)(106)

  step = 23
  print *, step, "v1b = v1c {v1b=", allocated(v1b), (allocated(v1b(i)%ivar),v1b(i)%ivar,i=1,size(v1b)), ", v1c=", allocated(v1c%ivar), v1c%ivar, "}"

  step = 24
  v1b = v1c


  step = 25
  print *, step, allocated(v4), allocated(v4a), allocated(v4b)

  step = 26
  print *, step, "v4 = [dka(4)(40000004)]"

  step = 27
  v4 = [dka(4)(40000004)]

  step = 28
  print *, step, "v4a = v4 {v4=", allocated(v4), (allocated(v4(i)%ivar),v4(i)%ivar,i=1,size(v4)), "}"

  step = 29
  v4a = v4
  v4a(1)%ivar = 30000003
  v4(1)%ivar = 20000002

  step = 30
  print *, step, "v4 = v4a {v4=", allocated(v4), (allocated(v4(i)%ivar),v4(i)%ivar,i=1,size(v4)), ", v4a=", allocated(v4a), (allocated(v4a(i)%ivar),v4a(i)%ivar,i=1,size(v4a)), "}"

  step = 31
  v4 = v4a

  step = 32
  print *, step, "v4b = [dka(4)(10000002), dka(4)(10000003), dka(4)(10000004), dka(4)(10000005)]"

  step = 33
  v4b = [dka(4)(10000002), dka(4)(10000003), dka(4)(10000004), dka(4)(10000005)]

  step = 34
  print *, step, "v4a = v4b {v4a=", allocated(v4a), (allocated(v4a(i)%ivar),v4a(i)%ivar,i=1,size(v4a)), ", v4b=", allocated(v4b), (allocated(v4b(i)%ivar),v4b(i)%ivar,i=1,size(v4b)), "}"

  step = 35
  v4a = v4b

  step = 36
  print *, step, "v4c = dka(1)(10000006)"

  step = 37
  v4c = dka(4)(10000006)

  step = 38
  print *, step, "v4a = v4c {v4a=", allocated(v4a), (allocated(v4a(i)%ivar),v4a(i)%ivar,i=1,size(v4a)), ", v4c=", allocated(v4c%ivar), v4c%ivar, "}"

  step = 39
  v4a = v4c

  print *, 91, allocated(v1),  v1%k,  (allocated(v1(i)%ivar),v1(i)%ivar,i=1,size(v1))
  print *, 92, allocated(v1a), v1a%k, (allocated(v1a(i)%ivar),v1a(i)%ivar,i=1,size(v1a))
  print *, 93, allocated(v1b), v1b%k, (allocated(v1b(i)%ivar),v1b(i)%ivar,i=1,size(v1b))
  print *, 94, allocated(v4),  v4%k,  (allocated(v4(i)%ivar),v4(i)%ivar,i=1,size(v4))
  print *, 95, allocated(v4a), v4a%k, (allocated(v4a(i)%ivar),v4a(i)%ivar,i=1,size(v4a))
  print *, 96, allocated(v4b), v4b%k, (allocated(v4b(i)%ivar),v4b(i)%ivar,i=1,size(v4b))

end program dtpIAAArray004
