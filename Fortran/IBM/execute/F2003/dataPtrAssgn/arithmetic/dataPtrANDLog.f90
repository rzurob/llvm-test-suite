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
!* data-ptr is a component of a derived-type which is the type of a component
!* of another derived type, multi-level of derived-types.  The last derived-type
!* contains an allocatable component which is the data-tar
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    type A
	logical*1, pointer :: p(:)
    end type

    type B
        type(A) :: a1
    end type

    type :: C
	type(B) :: b1
    end type

    type :: D
	type(C) :: c1
    end type

    type E
	type(D) :: d1
    end type

    type F
	type(E) :: e1
	logical*1, allocatable :: tar(:)
    end type

end module

program main

    use m

    type(F), target :: f1

    allocate(f1%tar(4), source = logical((/ .true.,.false., .true., .false. /), 1))
    f1%e1%d1%c1%b1%a1%p(1:4) => f1%tar(4:1:-1)

    if ( .not. associated(f1%e1%d1%c1%b1%a1%p,f1%tar(4:1:-1))) error stop 1
    if ( any (lbound(f1%e1%d1%c1%b1%a1%p) .ne. (/1/))) error stop 2
    if ( any (ubound(f1%e1%d1%c1%b1%a1%p) .ne. (/4/))) error stop 3

    print *, f1%e1%d1%c1%b1%a1%p
    print *, f1%e1%d1%c1%b1%a1%p .and. .true.

end program

