! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 291540)
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
    type base
        integer(4) :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(15) :: name

        contains

        procedure :: print => printChild
    end type

    type dataType
        class (base), allocatable :: data

        contains

        procedure :: print => printDataType
    end type

    contains

    subroutine printBase (b)
        class(base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printDataType (d)
        class (dataType), intent(in) :: d

        call d%data%print
    end subroutine
end module

program fmisc010b1_1
use m
    type (dataType) :: d1 (10)

    do i = 1, 10
        d1(i) = dataType(child(i, 'temp'))
    end do

    ! verify the results
    do i = 1, 10
        call d1(i)%print
    end do
end
