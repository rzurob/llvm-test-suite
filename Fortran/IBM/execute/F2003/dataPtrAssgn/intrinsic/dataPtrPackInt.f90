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
!* - data-pointer of type integer, as arg of pack
!* - lb is array element
!* - data-pointer as arg of elemental subroutine
!*
!234567890123456789012345678901234567890123456789012345678901234567890
    program main

	integer, pointer :: b1(:), b2(:)

	allocate(b2(10), source = (/ (i, i=1,10 )/) )

	b1(b2(10):) => b2

	if ( .not. associated(b1, b2) ) error stop 11
	if ( lbound(b1,1) /= 10 ) error stop 13
	if ( ubound(b1,1) /= 19 ) error stop 15

	call sub(b1, (/ (i, i=101,110 ) /))

	print *, pack(b1, (/(mod(i,2)==0,i=1,10 ) /), (/(b2(i),i=10,1, -1) /))

	print *, b2

	contains
	    elemental subroutine sub(a1, a2)
	        integer, intent(inout) :: a1
	        integer, intent(in) :: a2
		a1 = a2
	    end subroutine

    end program
