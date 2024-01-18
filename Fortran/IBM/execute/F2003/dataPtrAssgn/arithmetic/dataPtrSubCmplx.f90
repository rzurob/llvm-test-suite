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
!* - data-ptr with bound-remapping-list is nullified
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
    complex(8), target, allocatable :: c8(:)
    class(*), pointer :: ptr(:,:)

    allocate(c8(10), source = (/(cmplx(i*(-1)**mod(i,2),i*(-1)**mod(i+1,2), 8), i = 1, 10 )/))

    ptr(1:2, 2:6) => c8

    if ( .not. associated(ptr)) error stop 1
    if ( any (lbound(ptr) .ne. (/1,2/))) error stop 2
    if ( any (ubound(ptr) .ne. (/2,6/))) error stop 3

    select type(ptr)
	type is (complex(8))
	    ptr = ptr - cmplx(10,20,8)
    	    write (*, '("(",f10.6,", ", f10.6, ")")')  ptr
	class default
	    stop 5
    end select

    nullify(ptr)

    if ( associated(ptr)) error stop 7

end program
