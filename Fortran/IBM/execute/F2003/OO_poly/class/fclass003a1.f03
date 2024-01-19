! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/24/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (type with scalar pointer
!*                              components in defined assignment)
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

        contains

        final :: finalizeBase
        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character(20) name

        contains

        final :: finalizeChild
        procedure :: print => printChild
    end type

    type container
        class (base), pointer :: data

        contains

        final :: finalizeData
    end type

    interface assignment (=)
        subroutine co2Toco1 (co1, co2)
        import container
            class (container), intent(out) :: co1
            class (container), intent(in) :: co2
        end subroutine
    end interface

    contains

    subroutine finalizeData (d)
        type (container), intent(inout) :: d

        if (associated (d%data)) then
            print *, 'deallocating data'
            deallocate (d%data)
        end if
    end subroutine

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
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

program fclass003a1
use m
    class (base), pointer :: b1
    class (container), allocatable :: co1

    allocate (b1, source=child(1, 'b1'))
    allocate (co1)

    nullify (co1%data)

    print *, 'test1'
    co1 = container (b1)

    if (.not. associated (co1%data)) error stop 2_4

    call co1%data%print

    allocate (b1, source=base(100))


    print *, 'test2'

    co1 = container (b1)

    if (.not. associated (co1%data)) error stop 3_4

    call co1%data%print

    print *, 'end'
end

subroutine co2Toco1 (co1, co2)
use m, only : base, container
    class (container), intent(out) :: co1
    class (container), intent(in) :: co2

    if (associated (co2%data)) then
        allocate (co1%data, source=co2%data)
    end if
end subroutine
