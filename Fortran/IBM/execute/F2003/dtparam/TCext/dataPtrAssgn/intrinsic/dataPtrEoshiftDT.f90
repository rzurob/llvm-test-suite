! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrEoshiftDT.f
! opt variations: -qnol -qnodeferredlp

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
!* - data-pointer of derived type with allocatable component, as arg of Eoshift.
!* - two pointers points to the same address, as args of elemental subroutine
!*      which contains intrinsic assignment for each element
!* - -qalias = nostd
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    module m
        type base(n1,k1)    ! (20,4)
            integer, kind            :: k1
            integer, len             :: n1
            integer(k1), allocatable :: iP(:)
        end type

        contains
            elemental subroutine sub(a1, a2)
                type(base(*,4)), target, intent(inout) :: a1
                type(base(*,4)), target, intent(inout) :: a2
                a1 = a2
            end subroutine
    end module

    program main
        use m

        type(base(:,4)), pointer :: b1(:), b2(:)
        integer :: iT(10,10)
        type(base(:,4)), allocatable :: mid(:)

        iT = reshape((/ (i, i=1,100 ) /), (/10,10 /) )

        allocate(b2(20), source = (/ (base(20,4)(iT(i,:)), i=1,10 ), &
                (base(20,4)(iT(i,:)), i=1,10 )/) )

        b1(1:5) => b2(12::2)

        do i = 1, 5
            print *, b1(i)%ip
        end do

        if ( .not. associated(b1)) error stop 11
        if ( lbound(b1, 1) /= 1) error stop 13
        if ( ubound(b1, 1) /= 5) error stop 15

        call sub(b1, b2(2:10:2))

        mid = eoshift(b1, -1, b1(5))

        do i = 1, 5
            print *, mid(i)%ip
        enddo

        !print *, (/ ( mid(i)%ip, i=1,5)/)

    end program
