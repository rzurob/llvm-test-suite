!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal532.f
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
!*  DESCRIPTION                : final sub (finalization of allocated pointer
!*                               component in INTENT(OUT) dummy-arg)
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
    end type

    type container
        class (base), pointer :: data(:)

        contains

        final :: finalizeData, finalizeDataRank1
    end type

    contains

    elemental subroutine finalizeData (d)
        type (container), intent(inout) :: d
        integer(4) error

        if (associated (d%data)) deallocate (d%data, stat=error)
    end subroutine

    subroutine finalizeDataRank1 (d)
        type (container), intent(inout) :: d (:)

        integer(4) error

        do i = 1, size (d)
            if (associated (d(i)%data)) deallocate (d(i)%data, stat=error)
        end do
    end subroutine
end module

module m1
use m
    contains

    subroutine test1 (d)
        class (container), intent(out) :: d

        if (associated (d%data)) error stop 1_4
    end subroutine

    integer(4) function test2 (d)
        type (container), intent(out) :: d (:)

        test2 = 0

        do i = 1, size (d)
            if (associated (d(i)%data))     test2 = test2 + 1
        end do
    end function

    subroutine test3 (d)
        class (container), intent(out) :: d (:,:)

        do i = 1, ubound(d, 1)
            do j = 1, ubound (d, 2)
                if (associated (d(i,j)%data)) error stop 3_4
            end do
        end do
    end subroutine
end module

program ffinal532
use m1
    class (container), allocatable :: co1(:)
    type (container) co2

    class (container), pointer :: co3 (:,:)

    allocate (co1(3), co2%data(2), co3 (2,2))

    allocate (co1(1)%data(3), co1(2)%data(2), co1(3)%data(1))

    allocate (co3(1,1)%data(2), co3(2,2)%data(3), co3(2,1)%data(4))

    co3(1,2)%data => null()

    call test1 (co2)

    if (test2 (co1) /= 0) error stop 2_4

    call test3 (co3)
end
