! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr050a1_1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (poly allocatable
!                               array component with poly-array data-source; use
!                               rank-one arrays; use intrinsic assignment)
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
        integer(k1)      id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,15)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1)    name

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

        contains

        procedure :: print => printContainer
    end type

    contains

    subroutine printContainer (c)
        class (container(4)), intent(in) :: c

        if (allocated (c%data)) then
            print *, 'bounds:', lbound(c%data), ':', ubound(c%data)

            do i = lbound(c%data, 1), ubound(c%data, 1)
                call c%data(i)%print
            end do
        else
            print *, 'data not allocated'
        end if
    end subroutine
end module

program fconstr050a1
use m1
    class (base(4)), allocatable :: b1(:), b2(:), b3(:), b4(:), b5(:)

    type (container(4)) :: co (20)

    allocate (b1(-1:0), source = (/child(4,1,15)(1,'test1'), child(4,1,15)(2,'test2')/))
    allocate (b2(0:0), source=base(4) (100))
    allocate (b3(0:-10), source=child(4,1,15)(-1, 'no happening'))
    allocate (b4(3), source=(/(child(4,1,15)(i*10, 'test_array_3'), i=1,3)/))
    allocate (b5(2), source=child(4,1,15)(200, 'test_b5'))

    !! get the 1st 5 elements of co set up
    co(1) = container(4) (b1)
    co(2) = container(4) (b2)
    co(3) = container(4) (b3)
    co(4) = container(4) (b4)
    co(5) = container(4) (b5)

    print *, 'after the 1st assignment'

    do i = 1, 20
        write (*, '(a,i2,a1)', advance='no') 'element:', i, ' '
        call co(i)%print
    end do

    !! then use a FORALL statement to assign them to the last 5 element
    forall (i=1:10)   co(21-i) = co(i)

    print *, 'after the FORALL construct'
    print *, ''

    do i = 1, 20
        write (*, '(a,i2,a1)', advance='no') 'element:', i, ' '
        call co(i)%print
    end do

end
