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
!* - data-ptr is associated with FROM(10:1:-1)
!* - test if data-ptr is associated with TO(10:1:-1) after calling move_alloc
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

    integer, pointer :: ptr(:)
    integer, allocatable, target :: from(:)
    integer, allocatable, target :: to(:)

    allocate(from(10), source = (/(i,i=1,10 )/))

    ptr(0:) => from(10:1:-1)

    if ( .not. associated(ptr, from(10:1:-1))) error stop 1

    call move_alloc(from, to)

    if ( allocated(from) ) error stop 11
    if ( .not. allocated(to) ) error stop 13
    if ( .not. associated(ptr, to(10:1:-1))) error stop 17
    if ( lbound(ptr,1) /= 0) error stop 18
    if ( ubound(ptr,1) /= 9) error stop 19

    if ( any(ptr .ne. (/(i,i=10,1,-1)/))) error stop 20

end program
