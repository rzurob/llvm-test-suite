!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpIAAArraySection001
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-05-22
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : allocate DTP (kind) array and manipulate array sections via intrinsic assignment
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*  ADAPTED FROM               : dtpIAAArray001 (<-dtpIAABasic001)
!*
!*  DESCRIPTION
!*
!*  The simplest case of an allocatable DTP array with a kind parameter, we verify
!*  that the variables and allocation status and behaviour.
!*  This case extends Array001 by including array sections as vars, which should
!*  not experience reallocation, but will experience finalization on those parts
!*  which are assigned new values.  The two can be distinguished by which elements
!*  are given over to finalization (i.e., not the whole array).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAAArraySection001mod

  implicit none
  type dk(k)
     integer, kind :: k
     integer(k) :: ivar = 0
   contains
     final :: f1, f1arr, f4, f4arr
  end type dk

  integer, save :: step = 0

contains

  subroutine f1(a)
    type(dk(1)) :: a
    print *, step, "f1:", a%k, kind(a%ivar), a%ivar
  end subroutine f1

  subroutine f1arr(a)
    type(dk(1)) :: a(:)
    print *, step, "f1arr:", a%k, kind(a%ivar), size(a), a%ivar
  end subroutine f1arr

  subroutine f4(a)
    type(dk(4)) :: a
    print *, step, "f4:", a%k, kind(a%ivar), a%ivar
  end subroutine f4

  subroutine f4arr(a)
    type(dk(4)) :: a(:)
    print *, step, "f4arr:", a%k, kind(a%ivar), size(a), a%ivar
  end subroutine f4arr

end module dtpIAAArraySection001mod



program dtpIAAArraySection001

  use dtpIAAArraySection001mod
  implicit none

  type(dk(1)), allocatable :: v1(:), v1a(:), v1c(:)
  type(dk(1)) :: v1b
  type(dk(4)), allocatable :: v4(:), v4a(:), v4c(:), v4d(:)
  type(dk(4)) :: v4b

  ! assign array literals to different allocatable arrays, and then
  ! array sections - make sure space is only allocated when it should be.
  ! start at step 10 so that the later sort doesn't put "12 ..." before "3 ...":
  step = 10
  print *, step, allocated(v1), allocated(v1a), allocated(v1c)

  step = 11
  print *, step, "v1 = [dk(1)(101),dk(1)(102),dk(1)(103),dk(1)(104)]"
  step = 12
  v1 = [dk(1)(101),dk(1)(102),dk(1)(103),dk(1)(104)]

  step = 13
  print *, step, "v1a = v1 {v1=", allocated(v1), v1, "}"
  step = 14
  v1a = v1

  step = 15
  print *, step, "v1b = dk(1)(107)"
  step = 16
  v1b = dk(1)(107)

  step = 17
  print *, step, "v1c = [dk(1)(105),dk(1)(106)]"
  step = 18
  v1c = [dk(1)(105),dk(1)(106)]

  step = 19
  print *, step, "v1(2:3) = v1c {v1(2:3)=", v1(2:3), ", v1c=", v1c, "}"
  step = 20
  v1(2:3) = v1c
  step = 21
  print *, step, "v1=", v1, "/ v1a=", v1a ! yes, v1a, because we assigned it above, but this should be a copy

  step = 22
  print *, step, "v1a(1:3:2) = v1b {v1a(1:3:2)=", v1a(1:3:2), ", v1b=", v1b, "}"
  step = 23
  v1a(1:3:2) = v1b
  step = 24
  print *, step, "v1=", v1, "/ v1a=", v1a

  step = 25
  print *, step, "v1a(1:3:2) = v1c {v1a(1:3:2)=", v1a(1:3:2), ", v1c=", v1c, "}"
  step = 26
  v1a(1:3:2) = v1c
  step = 27
  print *, step, "v1=", v1, "/ v1a=", v1a

  step = 28
  print *, step, allocated(v4), allocated(v4a), allocated(v4c)

  step = 29
  print *, step, "v4 = [dk(4)(20000002), dk(4)(30000003), dk(4)(40000004)]"
  step = 30
  v4 = [dk(4)(20000002), dk(4)(30000003), dk(4)(40000004)]

  step = 31
  print *, step, "v4a = v4 {v4=", allocated(v4), v4, "}"
  step = 32
  v4a = v4

  step = 33
  print *, step, "v4b = dk(4)(50000005)"
  step = 34
  v4b = dk(4)(50000005)

  step = 35
  print *, step, "v4c = [dk(4)(60000006),dk(4)(70000007)]"
  step = 36
  v4c = [dk(4)(60000006),dk(4)(70000007)]

  step = 37
  print *, step, "v4(2:3) = v4c {v4(2:3)=", v4(2:3), ", v4c=", v4c, "}"
  step = 38
  v4(2:3) = v4c
  step = 39
  print *, step, "v4=", v4, "/ v4a=", v4a

  step = 40
  print *, step, "v4a(1:3:2) = v4b {v4a(1:3:2)=", v4a(1:3:2), ", v4b=", v4b, "}"
  step = 41
  v4a(1:3:2) = v4b
  step = 42
  print *, step, "v4=", v4, "/ v4a=", v4a

  step = 43
  print *, step, "v4a(1:3:2) = v4c {v4a(1:3:2)=", v4a(1:3:2), ", v4c=", v4c, "}"
  step = 44
  v4a(1:3:2) = v4c
  step = 45
  print *, step, "v4=", v4, "/ v4a=", v4a

  step = 46
  print *, step, "allocate (v4d(3), source = dk(4)(0))"
  step = 47
  allocate (v4d(3), source = dk(4)(0))
  step = 48
  print *, step, "v4d(:) = v4 {v4d(:)=", v4d(:), ", v4=", v4, "}"
  step = 49
  v4d(:) = v4
  step = 50
  print *, step, "v4d=", v4d, "/ v4=", v4

  ! Just use a higher number in place of "step":
  print *, 91, allocated(v1),  v1%k,  kind(v1%ivar),  v1
  print *, 92, allocated(v1a), v1a%k, kind(v1a%ivar), v1a
  print *, 93, allocated(v1c), v1c%k, kind(v1c%ivar), v1c
  print *, 94, allocated(v4),  v4%k,  kind(v4%ivar),  v4
  print *, 95, allocated(v4a), v4a%k, kind(v4a%ivar), v4a
  print *, 96, allocated(v4c), v4c%k, kind(v4c%ivar), v4c
  print *, 97, allocated(v4d), v4d%k, kind(v4d%ivar), v4d

end program dtpIAAArraySection001
