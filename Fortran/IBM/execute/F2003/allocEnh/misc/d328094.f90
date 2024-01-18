!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/15/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 328094)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point
        real x,y
    end type

    abstract interface
        elemental function unitPoint (p)
            import
            class(point), intent(in) :: p

            type(point) unitPoint
        end function
    end interface

    procedure(unitPoint), pointer :: gen

    contains

    subroutine Query
        print *, associated(gen)
    end subroutine
end module

!use m
!    gen => null()

!    call Query
end

