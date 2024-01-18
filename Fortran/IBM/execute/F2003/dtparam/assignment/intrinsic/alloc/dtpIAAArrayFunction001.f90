!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpIAAArrayFunction001
!*
!*  DATE                       : 2009-05-22
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : assign allocated DTP array (kind) returned from function
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpIAAArray001 (<-dtpIAABasic001)
!*
!*  DESCRIPTION
!*
!*  In a function, we assign values to the return variable and outside, assign the
!*  returned value to a variable.  Here, we have a type with kind.
!*  The function is defined in the module.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAAArrayFunction001mod

  implicit none
  type dk(k)
     integer, kind :: k
     integer(k) :: ivar = 0
  end type dk

contains

  function funk4a(i)
    integer, parameter :: k = 4
    type(dk(k)), allocatable :: funk4a
    integer(k) :: i
    funk4a = dk(k)(i)
  end function funk4a

  function funk1a(i)
    integer, parameter :: k = 1
    type(dk(k)), allocatable :: funk1a
    integer(k) :: i
    funk1a = dk(k)(i)
  end function funk1a

  function funk4(i)
    integer, parameter :: k = 4
    type(dk(k)) :: funk4
    integer(k) :: i
    funk4 = dk(k)(i)
  end function funk4

  function funk1(i)
    integer, parameter :: k = 1
    type(dk(k)) :: funk1
    integer(k) :: i
    funk1 = dk(k)(i)
  end function funk1

end module dtpIAAArrayFunction001mod



program dtpIAAArrayFunction001

  use dtpIAAArrayFunction001mod
  implicit none

  type(dk(1)), allocatable :: v1(:), v1a(:)
  type(dk(1)) :: v1b
  type(dk(4)), allocatable :: v4(:), v4a(:)
  type(dk(4)) :: v4b

  print *, allocated(v1), allocated(v1a)

  print *, "v1 = [funk1(101_1)]"
  v1 = [funk1(101_1)]

  print *, "v1a = v1 {v1=", allocated(v1), v1, "}"
  v1a = v1

  print *, "v1 = v1a {v1=", allocated(v1), v1, ", v1a=", allocated(v1a), v1a, "}"
  v1 = v1a

  print *, "v1b = funk1(102_1)"
  v1b = funk1(102_1)

  print *, "v1a = v1b {v1a=", allocated(v1a), v1a, ", v1b=", v1b, "}"
  v1a = v1b

  print *, allocated(v4), allocated(v4a)

  print *, "v4 = [funk4(20000002), funk4(30000003), funk4(40000004)]"
  v4 = [funk4(20000002), funk4(30000003), funk4(40000004)]

  print *, "v4a = v4 {v4=", allocated(v4), v4, "}"
  v4a = v4

  print *, "v4 = v4a {v4=", allocated(v4), v4, ", v4a=", allocated(v4a), v4a, "}"
  v4 = v4a

  print *, "v4b = funk4(50000005)"
  v4b = funk4(50000005)

  print *, "v4a = v4b {v4a=", allocated(v4a), v4a, ", v4b=", v4b, "}"
  v4a = v4b

  print *, allocated(v1),  v1%k,  kind(v1%ivar),  v1
  print *, allocated(v1a), v1a%k, kind(v1a%ivar), v1a
  print *, allocated(v4),  v4%k,  kind(v4%ivar),  v4
  print *, allocated(v4a), v4a%k, kind(v4a%ivar), v4a

  deallocate(v1, v4, v1a, v4a)

  ! repeat with allocatable-returning function
  print *, allocated(v1), allocated(v1a)

  print *, "v1 = [funk1a(101_1)]"
  v1 = [funk1a(101_1)]

  print *, "v1a = v1 {v1=", allocated(v1), v1, "}"
  v1a = v1

  print *, "v1 = v1a {v1=", allocated(v1), v1, ", v1a=", allocated(v1a), v1a, "}"
  v1 = v1a

  print *, "v1b = funk1a(102_1)"
  v1b = funk1a(102_1)

  print *, "v1a = v1b {v1a=", allocated(v1a), v1a, ", v1b=", v1b, "}"
  v1a = v1b

  print *, allocated(v4), allocated(v4a)

  print *, "v4 = [funk4a(20000002), funk4a(30000003), funk4a(40000004)]"
  v4 = [funk4a(20000002), funk4a(30000003), funk4a(40000004)]

  print *, "v4a = v4 {v4=", allocated(v4), v4, "}"
  v4a = v4

  print *, "v4 = v4a {v4=", allocated(v4), v4, ", v4a=", allocated(v4a), v4a, "}"
  v4 = v4a

  print *, "v4b = funk4a(50000005)"
  v4b = funk4a(50000005)

  print *, "v4a = v4b {v4a=", allocated(v4a), v4a, ", v4b=", v4b, "}"
  v4a = v4b

  print *, allocated(v1),  v1%k,  kind(v1%ivar),  v1
  print *, allocated(v1a), v1a%k, kind(v1a%ivar), v1a
  print *, allocated(v4),  v4%k,  kind(v4%ivar),  v4
  print *, allocated(v4a), v4a%k, kind(v4a%ivar), v4a


end program dtpIAAArrayFunction001
