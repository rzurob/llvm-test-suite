! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrGTInt.f
! opt variations: -qnol

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
!* - for defined =, LHS has type integer and RHS has a derived-type
!* - accordingly, the first arg associated with a component of the second arg
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base(n1,k1)    ! (20,4)
	integer, kind        :: k1
	integer, len         :: n1
	integer(k1), pointer :: ip(:)
    end type

    interface assignment(=)
	subroutine mydefine(a,b)
	    import base
	    type(base(*,4)), intent(in) :: b
	    integer, pointer, intent(out) :: a(:)
   	end subroutine
    end interface
end module

program main

    use m
    type(base(20,4)) :: b
    integer, pointer :: a(:)

    allocate(a(100), source = (/(i, i=2,200,2)/))

    b%ip => a

    a = b

    if ( .not. associated(a)) error stop 1
    if ( lbound(a,1) /= 100 ) error stop 2
    if ( ubound(a,1) /= 149 ) error stop 3
    if ( any ( a .gt. (/(i,i=200,1,-3) /))) error stop 4

end program

	subroutine mydefine(a,b)
	    use m, only : base
	    type(base(*,4)), intent(in) :: b
	    integer, pointer, intent(out) :: a(:)

            a(size(a):) => b%ip(ubound(b%ip,1):lbound(b%ip,1):-2)
   	end subroutine
