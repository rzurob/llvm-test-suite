!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal532a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalization of the data in FORALL
!                               construct)
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
        integer(4) id
    end type

    type container
        class (base), pointer :: data(:)

        contains

        final :: finalizeData
    end type

    class (container), allocatable :: co1_m(:)

    contains

    elemental subroutine finalizeData (d)
        type (container), intent(inout) :: d

        integer(4) error

        if (associated (d%data))    deallocate (d%data, stat=error)
    end subroutine
end module

program ffinal532a
use m
    type (container) co1(3), co2(2,2)
    type (container), parameter :: co_const = container(null())

    allocate (co1(1)%data(2:3), co1(2)%data(3))

    nullify (co1(3)%data)

    allocate (co1_m(3))

    allocate (co1_m(1)%data(1), co1_m(2)%data(10), co1_m(3)%data(1))

    forall (i=1:3)
        co1(i) = co1_m(i)
    end forall

    do i = 1, 3
        if (.not. associated (co1(i)%data, co1_m(i)%data)) error stop 1_4
    end do


    !! assign co2

    allocate (co2(1,1)%data(2), co2(1,2)%data(3), co2(2,1)%data(4))

    co2(2,2)%data => null()

    forall (i=1:2,j=1:2)
        co2(i,j) = co_const
    end forall

    do i =1,2
        do j = 1,2
            if (associated (co2(i,j)%data)) error stop 2_4
        end do
    end do
end
