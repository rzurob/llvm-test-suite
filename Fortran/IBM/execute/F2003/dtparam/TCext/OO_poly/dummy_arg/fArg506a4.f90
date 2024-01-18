! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg506a4.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/07/2005
!*
!*  DESCRIPTION                : argument association (INTENT(OUT) and default
!                               initializations)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'
        type(base(k1))            :: data (3)

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name

        do i = 1, 3
            call b%data(i)%print
        end do
    end subroutine
end module

program fArg506a4
use m
    class (base(4)), allocatable :: b1 (:,:)

    type (child(4,1,20)) :: c1 (4)

    c1%id = (/1,2,3,4/)
    c1%name = (/'c1_1','c1_2','c1_3','c1_4'/)
    c1(1)%data = (/(base(4) (i*10), i=1,3)/)
    c1(2)%data = (/(base(4) (i*20), i=1,3)/)
    c1(3)%data = (/(base(4) (i*30), i=1,3)/)
    c1(4)%data = (/(base(4) (i*40), i=1,3)/)

    allocate (b1(2,2), source=reshape(c1, (/2,2/)))

    call test1 (b1(1,:))

    call b1(2,1)%print
    call b1(2,2)%print

    contains

    subroutine test1 (b)
        class (base(4)), intent(out) :: b (2)

        do i = 1, size (b)
            call b(i)%print
        end do
    end subroutine
end
