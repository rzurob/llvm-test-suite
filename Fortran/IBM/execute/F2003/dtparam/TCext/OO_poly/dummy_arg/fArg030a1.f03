! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg030a1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/11/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (poly allocatable
!                               component in argument association; use poly
!                               actual-arg with non-poly dummy-arg)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = 1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'no-name'

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
    end subroutine
end module

module m1
use m
    type container(k3)    ! (4)
        integer, kind                :: k3
        class(base(k3)), allocatable :: data(:)
    end type

    contains

    subroutine printData (co)
        type (container(4)), intent(in) :: co(5)

        do i = 1, 5
            if (allocated(co(i)%data)) then
                print *, 'data element: ', i

                do j = lbound(co(i)%data,1), ubound(co(i)%data,1)
                    call co(i)%data(j)%print
                end do
            end if
        end do
    end subroutine
end module

program fArg030a
use m1
    class(container(4)), allocatable :: co1(:)

    allocate (co1(10))

    do i = 1, 5
        allocate (co1(i)%data(i:2*i), source=(/(child(4,1,20)(j, 'test_part_1'), &
                        j=i,2*i)/))
    end do


    call printData (co1)
end
