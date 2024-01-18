! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 4/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (structure constructor
!*                               can be hidden/overridden by generics)
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
        integer*4 :: id
        integer*4, allocatable, private :: data

        contains

        procedure :: print => printBase
    end type

    interface base
        function makeBase (i1, i2)
        import base
            type (base) :: makeBase
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        if (allocated (b%data)) then
            print *, b%id, b%data
        else
            print *, b%id
        end if
    end subroutine

    subroutine assgnID (b1, i1)
        class (base), intent(inout) :: b1
        integer*4, intent(in) :: i1

        if (.not. allocated(b1%data))   allocate(b1%data)

        b1%data = i1
    end subroutine
end module

program fconstr049
use m
    type (base) :: b1

    b1 = base(1, 2)

    call b1%print

end

function makeBase (i1, i2)
use m, only:base, assgnID
    type (base) :: makeBase

    call assgnID (makeBase, i2)

    makeBase%id = i1
end function
