!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - multi-level derived-types with pointer or allocatable components
!*     data-ptr & data-tar are components at diff levels.
!* - apply intrinsic allocatable enhancement.
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module m
    type A
        real, pointer :: ptr(:,:)
    end type

    type B
        class(A), allocatable :: a1
    end type

    type, extends(B) :: C
        real, allocatable :: tar(:)
    end type

    type D
        class(C), pointer :: c1
    end type

end module

program main

    use m

    type(D), allocatable :: d1, d2

    allocate(d1)

    allocate(d1%c1)

    allocate(d1%c1%tar(20), source=(/( real(max(i*i, i+i),4), i=-10,9)/))

    allocate(d1%c1%a1)

    d1%c1%a1%ptr(2:6,0:3) => d1%c1%tar

    if ( .not. associated(d1%c1%a1%ptr)) error stop 2
    if ( any(lbound(d1%c1%a1%ptr) .ne. (/ 2,0/))) error stop 5
    if ( any(ubound(d1%c1%a1%ptr) .ne. (/ 6,3/))) error stop 7

    d2 = d1

    if ( any(d2%c1%a1%ptr /= reshape((/( real(max(i*i, i+i),4), i=-10,9)/), (/5,4/) ))) error stop 10

end program

