! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrReshapeDT.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrReshapeDT.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-pointer of derived-type, as arg of reshape
!* - data-target is tar1%b1%a1; tar1 and a1 are scalar of DTs, b1 is array of DT
!* - a1 has integer allocatable component
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    type base(n1,k1)    ! (20,4)
	integer, kind            :: k1
	integer, len             :: n1
	integer(k1), allocatable :: id
    end type

    type A(k2,n2)    ! (4,20)
        integer, kind     :: k2
        integer, len      :: n2
        type(base(n2,k2)) :: a1
    end type

    type :: B(k3,n3)    ! (4,20)
        integer, kind  :: k3
        integer, len   :: n3
        type(A(k3,n3)) :: b1(10)
    end type
end module

    program main

        use m

        class(base(:,4)), pointer :: p1(:)
        type(B(4,20)), target :: tar1
        type(base(:,4)), allocatable :: res(:,:)
        integer i

        do i = 1, 10
!        tar1%b1 = (/ ( A(4,20)(base(20,4)(i)),i= 1, 10 ) /)
            tar1%b1(i) = A(4,20)(base(20,4)(i))
        end do

        p1(-1:8) => tar1%b1%a1

        if ( .not. associated(p1, tar1%b1%a1)) stop 11
        if (lbound(p1,1) /= -1 ) stop 13
        if (ubound(p1,1) /= 8 ) stop 15

	print *, (/ (p1(i)%id, i= -1,8)/)

	res = reshape(p1,(/2, 5/) )

	print *, shape(res)

	do i = 1, 2
	    do j = 1, 5
                print *, (/ (res(i,j)%id ) /)
	    enddo
	end do

    end program
