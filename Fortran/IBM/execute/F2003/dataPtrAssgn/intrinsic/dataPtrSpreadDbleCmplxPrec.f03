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
!* - data-pointers are components of DT, type double complex/precision
!* - the same data-pointers are passed as an actual arg of subprogram's
!* -       two dummy args
!* - inside subprogram(elemental/nonelemental), dummy arg is redefined
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    module m
	type base
	    double complex, pointer :: dcp(:)
	end type
    end module

    module n
	use m

	type, extends(base) :: child
	    double precision, pointer :: dpp(:)
	end type
    end module

    module k
	use n

	contains
	    elemental subroutine sub_dp(a1, a2)
	        double precision, intent(inout) :: a1
	        double precision, intent(inout) :: a2
		a1 = a2
	    end subroutine

	    subroutine sub_dc(a1, a2)
	        double complex, intent(inout) :: a1(:)
	        double complex, intent(inout) :: a2(:)
		a2 = a2(ubound(a1,1):lbound(a1,1):-1)
	    end subroutine
    end module

    program main
	use k

	type(child) :: c

	allocate(c%dcp(10), source = (/(cmplx(i,-i,8), i=1,10 )/) )
	allocate(c%dpp(10), source = (/(real(c%dcp(i)), i=1,10 )/) )

	! double complex
	c%dcp(11:) => c%dcp(::2)

	if ( .not. associated(c%dcp)) error stop 11
	if ( lbound(c%dcp,1) /= 11 ) error stop 13
	if ( ubound(c%dcp,1) /= 15 ) error stop 15

	call sub_dc(c%dcp, c%dcp)

	write (*, '("(",f10.3,", ", f10.3, ")")')  c%dcp
	write (*, '("(",f10.3,", ", f10.3, ")")')  spread(c%dcp, 1,3)

	! double precision
	c%dpp(1:8) => c%dpp

	if ( .not. associated(c%dpp)) error stop 21
	if ( lbound(c%dpp,1) /= 1 ) error stop 23
	if ( ubound(c%dpp,1) /= 8 ) error stop 25

	call sub_dp(c%dpp, c%dpp)
	write(*, '(4f10.3)') c%dpp
	write(*, '(4f10.3)') spread(c%dpp, 1, 3)

    end program