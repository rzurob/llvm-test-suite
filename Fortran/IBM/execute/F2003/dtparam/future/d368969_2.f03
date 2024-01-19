! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2009-10-01
!*
!*  DESCRIPTION                : defect 368969. case 2: fail at run-time.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(n1)    ! (4,15)
        integer, len  :: n1
        character(n1) :: name

        contains

        procedure :: print => printChild
    end type

    type dataType(k2)    ! (4)
        integer, kind                :: k2
        class(base(k2)), allocatable :: data

        contains

        procedure :: print => printDataType
    end type

    contains

    subroutine printBase (b)
        class(base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printDataType (d)
        class (dataType(4)), intent(in) :: d

        call d%data%print
    end subroutine
end module

program fmisc010b1
use m
    type (dataType(4)) :: d1 (10)

    d1 = (/(dataType(4)(child(4,15)(i, 'temp')), i = 1, 10)/)

    ! verify the results
    do i = 1, 10
        call d1(i)%print
    end do
end
