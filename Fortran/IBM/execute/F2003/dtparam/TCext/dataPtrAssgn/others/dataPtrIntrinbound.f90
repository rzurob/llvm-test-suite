! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/dataPtrAssgn/others/dataPtrIntrinbound.f
! opt variations: -qnok -qnol

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrIntrinbound.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - ub of data-pointer is intrinsic func
!* - data-pointer/target have attributes save, public, volatile
!* -
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    type base(n1,k1)    ! (20,4)
	integer, kind :: k1
	integer, len  :: n1
	integer(k1)   :: p(10)
    end type

    type another(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        class(*), pointer :: p(:)
    end type

    type(another(4,20)), public, save, volatile, target :: a

end module

program main
    use m

    type(base(20,4)), volatile, target :: b

    data ( b%p(i), i = 2,6) / 1,2,3,4,5/

    a%p(ishft(1, 2):int(7.0)) => b%p(b%p(3) : b%p(6))

    if ( .not. associated(a%p) ) stop 9
    if ( lbound( a%p, 1) /= 4 ) stop 19
    if ( ubound( a%p, 1) /= 7 ) stop 29

    select type ( y => a%p)
	type is (integer)
	    if ( any(y .ne. (/ 1, 2, 3, 4 /)) ) stop 39
	class default
	    stop 22
	end select

end
