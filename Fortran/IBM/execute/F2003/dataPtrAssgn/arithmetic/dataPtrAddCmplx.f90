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
!* - data-ptr is redefined by where statement
!* - data-ptr is used by mask-expr of where construct
!* - data-ptr is of type class(*)
!* - where statement  appears in select type construct
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
    complex(4), target, allocatable :: c4(:)
    class(*), pointer :: ptr(:)

    allocate(c4(10), source = (/(cmplx(i*(-1)**mod(i,2),i*(-1)**mod(i+1,2), 4), i = 1, 10 )/))

    ptr((-1)**mod(1,2):) => c4(10:1:-1)

    if ( .not. associated(ptr, c4(10:1:-1))) error stop 1
    if ( lbound(ptr,1) /= -1 ) error stop 2
    if ( ubound(ptr,1) /= 8 ) error stop 3

    select type(ptr)
	type is (complex(4))
            where ( real(ptr) < 0.0  )
        	ptr = -ptr
		ptr = ptr + ptr
            end where
	class default
	    stop 5
    end select

    select type(ptr)
	type is (complex(4))
    	    write (*, '("(",f10.6,", ", f10.6, ")")')  ptr
	class default
	    stop 5
    end select

    write (*, '("(",f10.6,", ", f10.6, ")")')  c4

end program
