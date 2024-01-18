!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/02/2005
!*
!*  DESCRIPTION                : miscellaneous (defect 312366)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    subroutine bad(x)
        class(*), intent(in) :: x(:)

        associate (y => x)
            entry bad2 (y)      !<-- illegal
        end associate
    end subroutine


    subroutine bad1
        class(*), pointer :: x(:)

        allocate (x(10), source=10)

        select type (x)
            entry z()           !<-- illegal
            type is (integer)
                entry y()       !<-- illegal
        end select
    end subroutine
end module

end
