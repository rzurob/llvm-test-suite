!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrSubCmplx.f 
!*
!*  PROGRAMMER                 : Michelle Zhang
!*  DATE                       : Aug 31, 2006
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
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

    if ( .not. associated(ptr)) stop 1
    if ( any (lbound(ptr) .ne. (/1,2/))) stop 2 
    if ( any (ubound(ptr) .ne. (/2,6/))) stop 3 

    select type(ptr)
	type is (complex(8)) 
	    ptr = ptr - cmplx(10,20,8) 
    	    write (*, '("(",f10.6,", ", f10.6, ")")')  ptr 
	class default
	    stop 5 
    end select

    nullify(ptr)

    if ( associated(ptr)) stop 7

end program
