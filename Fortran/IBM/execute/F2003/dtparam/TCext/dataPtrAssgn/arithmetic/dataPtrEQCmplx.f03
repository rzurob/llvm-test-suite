! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrEQCmplx.f
! opt variations: -qnok -ql

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
!* - defined-assignment for a derived-type obj on LHS & a complex type obj
!*    on RHS of "=". In the sub for defined-assgn =, the component pointer is
!*    associated with the complex type obj
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type base(k1)    ! (4)
        integer, kind :: k1
	class(*), pointer :: ptr(:,:)
    end type
end module

program main

    use m
    interface assignment(=)
	subroutine mydefine(a,b)
	    import base
	    complex(4), target, intent(in) :: b(100)
	    type(base(4)), intent(out) :: a
   	end subroutine
    end interface

    complex*8, target  ::  b(100)
    type(base(4)) :: a

    b = (/( cmplx(i-1,i+1,4), i=1,100  )/)

    a = b

end program

subroutine mydefine(a,b)
    use m, only:base

    complex(4), target, intent(in) :: b(100)
    type(base(4)), intent(out) :: a

    a%ptr(2:11,3:12) => b(100:1:-1)

    if ( .not. associated(a%ptr)) error stop 1
    if ( any (lbound(a%ptr) .ne. (/2,3/))) error stop 2
    if ( any (ubound(a%ptr) .ne. (/11,12/))) error stop 3

    select type (x => a%ptr)
    	type is (complex(4))
	    write (*, '("(",f10.6,", ", f10.6, ")")') x
	    if ( any (x == reshape((/(cmplx(i-1,i+1,4),i=100,1,-1)/), (/10,10/)) .neqv. .true. )) error stop 6
        class default
	    stop 8
    end select
end subroutinE