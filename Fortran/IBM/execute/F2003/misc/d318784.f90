!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/18/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 318784)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(4), allocatable :: d
    end type

    type container
        class(base), allocatable :: data
    end type

    contains

    recursive subroutine printCo (co)
        !type (container), value :: co
        type (container) :: co

        type(container) temp

        if (allocated(co%data)) then
            if (allocated(co%data%d)) then
                if (co%data%d > 0) then

                    write (*, '(f10.3)') co%data%d

                    temp = container(base(co%data%d - 1.0))

                    call printCo (temp)
                else
                    print *, 'finished'
                end if
            else
                print *, 'd is not allocated'
            end if
        end if
    end subroutine
end module

use m
    call printCo (container(base(3.5)))
end
