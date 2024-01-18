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
!* - data-tar has allocatable attr, which is redefined by intrinisc =, due to
!*   the size changed. Test the association status of data-ptr &data-target
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

    type base
	integer id
    end type

    class(base), pointer :: ptr(:)
    type(base), target, allocatable :: tar(:)

    allocate(tar(10), source=(/ ( base(i), i=1,10)/))

    ptr(2:11) => tar

    if (.not. associated(ptr,tar)) error stop 1
    if ( lbound(ptr,1) /= 2) error stop 2
    if ( ubound(ptr,1) /= 11) error stop 3
    if ( any(ptr%id .ne. (/(i,i=1,10)/))) error stop 5

    tar = (/ ( base(i), i=1,11)/)
    if ( associated(ptr,tar)) error stop 6

end program

