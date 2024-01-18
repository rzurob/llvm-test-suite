!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrMatmulInt.f 
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
!* - data-pointer of type integer as arg of matmul
!* - the same pointers as actual args of two dummy args of module procedure
!*            which contains intrinsic assignment for two pointer args
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    module m

	contains
	     subroutine sub(a1, a2)
	        integer, pointer, intent(inout) :: a1(:)
	        integer, intent(in) :: a2(:)
		a1 = a2
	    end subroutine

    end module
		
    program main
	use m

	integer, pointer :: b1(:), b2(:)

	allocate(b2(10), source = (/ (i, i=1,10 )/) )

	b1(11:20) => b2

	if ( .not. associated(b1) ) stop 21 
	if ( lbound(b1,1) /= 11 ) stop 31 
	if ( ubound(b1,1) /= 20 ) stop 41

	call sub(b1, (/ (b2(i), i=10,1,-1 ) /))

	print *, b2
	print *, matmul(b1, reshape((/(-1, i=1,10 ) /), (/10,1/)))

    end program
