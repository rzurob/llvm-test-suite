! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg037.f
!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 05/17/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : argument association (dummy procedure used as
!                               the defined assignment)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1)    name
    end type

    type container(k3)    ! (4)
        integer, kind            :: k3
        class(base(k3)), pointer :: data(:) => null()

        contains

        final :: finalizeContainer
    end type

    interface
        subroutine copyCo1fromCo2 (co1, co2)
        import container
            class(container(4)), intent(out) :: co1
            class(container(4)), intent(in) :: co2
        end subroutine
    end interface

    contains

    subroutine finalizeContainer (co)
        type (container(4)), intent(inout) :: co

        if (associated(co%data)) deallocate (co%data)
    end subroutine

    subroutine copyData (co1, co2, assgn)
        class(container(4)), intent(out) :: co1
        class(container(4)), intent(in) :: co2

        interface assignment(=)
            subroutine assgn (co1, co2)
            import container
                class(container(4)), intent(out) :: co1
                class(container(4)), intent(in) :: co2
            end subroutine
        end interface

        co1 = co2
    end subroutine
end module

program fArg037
use m
    class(container(4)), allocatable :: co1, co2, co3

    procedure (copyCo1fromCo2) assgn1, assgn2

    allocate (co1, co2, co3)
    allocate (co2%data(10), source=child(4,1,20)(10, 'test 101'))
    allocate (co1%data(2))

    call copyData (co1, co2, assgn1)

    if (.not. associated (co1%data)) error stop 1_4

    if (size(co1%data) /= 10) error stop 2_4

    select type (x => co1%data)
        type is (child(4,1,*))
            do j = 1, 10
                if ((x(j)%id /= 10) .or. (x(j)%name /= 'test 101')) &
                        error stop 3_4
            end do
        class default
            error stop 4_4
    end select

    call copyData (co1, co3, assgn2)

    if (.not. associated (co1%data)) error stop 5_4

    if (size(co1%data) /= 0) error stop 6_4

    call copyData (co1, co3, assgn1)

    if (associated (co1%data)) error stop 7_4
end


subroutine assgn1 (co1, co2)
use m
    class(container(4)), intent(out) :: co1
    class(container(4)), intent(in) :: co2

    if (associated (co2%data)) then
        allocate (co1%data(size(co2%data)), source=co2%data)
    end if
end subroutine


subroutine assgn2 (co1, co2)
use m
    class(container(4)), intent(out) :: co1
    class(container(4)), intent(in) :: co2

    if (associated (co2%data)) then
        allocate (co1%data(size(co2%data)), source=co2%data)
    else
        allocate (co1%data(0))
    end if
end subroutine
