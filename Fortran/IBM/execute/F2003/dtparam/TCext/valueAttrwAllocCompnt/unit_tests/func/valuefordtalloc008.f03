! GB DTP extension using:
! ftcx_dtp -ql -qreuse=self /tstdev/F2003/valueAttrwAllocCompnt/unit_tests/func/valuefordtalloc008.f
! opt variations: -qnol -qreuse=none

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan, 20, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : value attribute for derived types with allocatable components
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=polymorphic
!*
!*  DESCRIPTION                : functional testing of value attribute
!*                               for derived types with allocatable
!*                               components
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  module m
    type A(n1,k1)    ! (20,4)
      integer, kind            :: k1
      integer, len             :: n1
      integer(k1)              :: x
      integer(k1), allocatable :: y
      integer(k1), allocatable :: z

      contains

        procedure ::  sub

    end type

    contains

      subroutine sub(A1,A2)
        class(A(*,4)) :: A1
        type(A(20,4)) :: A2
        value :: A2

        if((A1%x .ne. 1) .or. (A1%y .ne. 2) .or. (A1%z  .ne. 3)) error stop 1_4
        if((A2%x .ne. 4) .or. (A2%y .ne. 5) .or. (A2%z  .ne. 6)) error stop 2_4

        A1%x=0
        A1%y=0
        A1%z=0

        A2%x=0
        A2%y=0
        A2%z=0

      end subroutine

  end module

  use m

  type(A(20,4)) :: A3, A4

  A3%x=1
  allocate(A3%y)
  allocate(A3%z)
  A3%y=2
  A3%z=3

  A4%x=4
  allocate(A4%y)
  allocate(A4%z)
  A4%y=5
  A4%z=6

  call sub(A3,A4)

  if((A4%x .ne. 4) .or. (A4%y .ne. 5) .or. (A4%z  .ne. 6)) error stop 3_4
  if((A3%x .ne. 0) .or. (A3%y .ne. 0) .or. (A3%z  .ne. 0)) error stop 4_4

end program






