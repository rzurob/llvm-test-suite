!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc001a3.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (type-spec in ALLOCATE; use real and
!                               logical types)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fallo001a3
    class(*), allocatable :: x0, x1, x2(:), x3(:,:)
    real(4), pointer :: r1
    real (kind=16), allocatable :: r2 (:)
    real*8, pointer :: r3

    logical (1), allocatable :: l1
    logical*2, pointer :: l2(:)
    logical(kind=4), pointer :: l3(:,:)
    logical (8), allocatable :: l4

    logical precision_r4, precision_r8, precision_r6

    !! tests the type-spec for real in allocate
    allocate (real :: r1, x2(2))

    allocate (real*8 :: r3, x1)

    deallocate (x1, x2)

    allocate (real(kind=16) :: r2 (10), x1)

    r1 = 1.0e-2
    r3 = 1.0d-2
    r2 = (/(i*1.q-1, i=1,10)/)

    deallocate (x1)

    !! test the type-spec for logical in allocate
    allocate(logical(kind=1) :: l1, x1)
    allocate (logical(2) :: l2(2:4), x2(-2:0))
    allocate (logical :: x0, l3(1,2))
    allocate (logical*8 :: x3(2,3), l4)

    l1 = .true.
    l2 = .true.
    l3 = .true.
    l4 = .false.

    if (.not. precision_r4(r1, .01)) error stop 1_4

    do i = 1, 10
        if (.not. precision_r6(r2(i), .1q0*i)) error stop 2_4
    end do

    if (.not. precision_r8 (r3, .01d0)) error stop 3_4

    if ((.not. l1) .or. l4) error stop 4_4

    if ((.not. all (l2)) .or. (.not. all (l3))) error stop 5_4
end
