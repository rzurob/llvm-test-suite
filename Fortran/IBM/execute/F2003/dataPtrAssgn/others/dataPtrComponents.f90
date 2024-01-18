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
!* - data-ptr has bounds-remapping-list
!* - data-ptr and data-tar are components of two different derived-type,
!*   where one derived-type is an extended DT from the other and the parent
!*   DT contains the pointer component
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

    type :: D
        integer, pointer :: p(:,:,:)
    end type

    type, extends(D) :: E
        integer, allocatable :: tar(:)
    end type

    type(E), target :: e1

    allocate(e1%tar(3), source = (/ 1,2,3 /) )

    e1%p(3:3,0:0,4:6) => e1%tar

    if ( .not. associated(e1%p)) stop 1
    if ( any (lbound(e1%p) .ne. (/3,0,4/))) stop 3
    if ( any (ubound(e1%p) .ne. (/3,0,6/))) stop 5

    print *, e1%p

end program
