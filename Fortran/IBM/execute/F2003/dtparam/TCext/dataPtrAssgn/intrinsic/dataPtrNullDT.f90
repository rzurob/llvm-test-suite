! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qnodefaultpv -qdeferredlp /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrNullDT.f
! opt variations: -qnock -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

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
!* - data-pointer of class(*), as arg of null
!* - the left part-name of data-target is an array of derived type
!* - the right part-name of data-target is a component of another derived type
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    type base(k1,n1)    ! (1,1)
	integer, kind                          :: k1
	integer, len                           :: n1
	character(kind=k1,len=n1), allocatable :: ch
    end type

    type A(k2,n2,k3,n3)    ! (4,20,1,1)
        integer, kind     :: k2,k3
        integer, len      :: n2,n3
        type(base(k3,n3)) :: a1
    end type

    type :: B(k4,n4,k5,n5)    ! (4,20,1,1)
        integer, kind        :: k4,k5
        integer, len         :: n4,n5
        type(A(k4,n4,k5,n5)) :: b1
    end type
end module

    program main

        use m

        class(*), pointer :: p1(:)
        type(B(4,20,1,1)), target :: tar1(20)
        type(base(1,:)), allocatable :: res(:,:,:)
        integer i

        do i = 66, 85
!        tar1 = (/ ( B(4,20,1,1)(A(4,20,1,1)(base(1,1)(achar(i)))),i=66, 85 ) /)
        tar1(i-65) = B(4,20,1,1)(A(4,20,1,1)(base(1,1)(achar(i))))
        end do

	do i = 1, 20
	    print *,  tar1(i)%b1%a1%ch
	end do

	! print *, (/ (tar1(i)%b1%a1%ch, i= 11,30)/)

        p1(11:) => tar1%b1%a1

        if ( .not. associated(p1, tar1%b1%a1)) error stop 11
        if (lbound(p1,1) /= 11 ) error stop 13
        if (ubound(p1,1) /= 30 ) error stop 15

	select type (p1)
	    type is (base(1,*))
	         print *, (p1(i)%ch, i= 11,30)
	    class default
		stop 25
	end select

	if ( associated( null(p1) )) error stop 35

    end program
