!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fclass003a3.f
! %VERIFY: fclass003a3.out:fclass003a3.vf
! %STDIN:
! %STDOUT: fclass003a3.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/24/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (defined assignment involving
!                               poly-pointer array components and finalization)
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
        integer(8) id

        contains

        final :: finalizeBase
        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character (18) :: name

        contains

        final :: finalizeChild
        procedure :: print => printChild
    end type

    type container
        class (base), pointer :: data(:)

        contains

        final :: finalizeData
    end type


    interface assignment (=)
        subroutine co2Assgn2co1 (co1, co2)
        import container
            class (container), intent(out) :: co1
            class (container), intent(in) :: co2
        end subroutine
    end interface


    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(inout) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeData (d)
        type (container), intent(inout) :: d

        if (associated (d%data)) then
            print *, 'deallocating data'
            deallocate (d%data)
        end if
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module


subroutine co2Assgn2co1 (co1, co2)
use m, only: container, base
    class (container), intent(out) :: co1
    class (container), intent(in) :: co2

    if (associated (co2%data)) then
        allocate (co1%data(size(co2%data)), source= &
            co2%data(ubound(co2%data,1):lbound(co2%data,1):-1))
    end if
end subroutine

program fclass003a3
use m
    class (base), pointer :: b1(:)
    class (container), allocatable :: co1, co2

    allocate (b1(2), source=(/child(1, 'b1_1'), child(2, 'b1_2')/))

    allocate (co1, co2)

    co1%data => null()

    co2%data => b1

    !! test 1: user defined assignment
    print *, 'test 1'
    co1 = co2

    if (.not. associated (co1%data)) error stop 1_4
    if (associated (co1%data, co2%data)) error stop 2_4
    if (size (co1%data) /= 2) error stop 3_4

    call co1%data(1)%print
    call co1%data(2)%print


    !! test 2: still defined assignment
    print *, 'test 2'

    deallocate (co2%data)

    allocate (co2%data(1), source=base(100))

    co1 = co2

    if (.not. associated (co1%data)) error stop 4_4
    if (associated (co1%data, co2%data)) error stop 5_4
    if (size (co1%data) /= 1) error stop 6_4

    call co1%data(1)%print

    print *, 'end'
end
