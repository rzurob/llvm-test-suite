!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/01/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Keyword specify an invalid component name
!                               in component-spec.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data
        integer(k) :: id (n)
    end type

    type, extends(base) :: child (l)
        integer, len :: l

        character(l) :: name
    end type

    type (child(8,:,:)), allocatable :: c1
end module

program dtparamConstr016d
use m
    class(base(4,:)), pointer :: b1

    !! structure constructor contains invalid keyword
    allocate (b1, source=child(4, 20, 30)(data=1.0, id=(/(i, i=1,20)/), &
            child='b1'))

    allocate (child(8, 55, 25) :: c1)

    !! structure constructor contains invalid keyword
    c1 = child(8, 55, 25)(base%data=2.0d0, c1%id=(/(exp(i*1.0d0), i=1,55)/), &
            name='c1')
end
