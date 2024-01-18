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
!* - data-ptr has bounds-spec-list
!* - data-ptr is a component of a derived-type which is the type of a component
!*   of another derived type which contains an allocatable target component
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

   type A
        integer, pointer :: p(:)
    end type

    type F
        type(A) :: a1
        integer, allocatable :: tar(:)
    end type

    type(F), target :: f1

    allocate(f1%tar(4), source = (/ 1,2,3,4 /))

    f1%a1%p(1:) => f1%tar

    if ( .not. associated(f1%a1%p, f1%tar))error stop 1
    if ( any (lbound(f1%a1%p) .ne. (/1/))) error stop 2
    if ( any (ubound(f1%a1%p) .ne. (/4/))) error stop 3

    print *, f1%a1%p

end program

