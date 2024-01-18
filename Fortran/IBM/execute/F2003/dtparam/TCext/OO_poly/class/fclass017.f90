! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass017.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/10/2005
!*
!*  DESCRIPTION                : class keyword (defined assignment uses the
!                               intrinsic assignment)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind            :: k1
        complex(k1), allocatable :: data(:)

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (8,1,20)
        integer, kind                          :: k2
        integer, len                           :: n1
        character(kind=k2,len=n1), allocatable :: name

        contains

        procedure :: print => printChild
    end type

    interface assignment(=)
        subroutine assgnB1B2 (b1, b2)
        import base
            class (base(8)), intent(out) :: b1
            class (base(8)), intent(in) :: b2
        end subroutine
    end interface

    contains

    subroutine  printBase (b)
        class (base(8)), intent(in) :: b

        if (allocated(b%data)) then
            print *, 'bounds of data:', lbound(b%data), ubound(b%data)

            write (*, '(3("(",f10.2,",",f10.2, ") "))') b%data
        else
            print *, 'data not allocated'
        end if
    end subroutine

    subroutine printChild (b)
        class(child(8,1,*)), intent(in) :: b

        call b%base%print

        if (allocated (b%name)) then
            print *, b%name
        else
            print *, 'name not allocated'
        end if
    end subroutine
end module

subroutine assgnB1B2 (b1, b2)
use m, only: base, child
    class (base(8)), intent(out) :: b1
    class (base(8)), intent(in) :: b2

    if (.not. same_type_as (b1, b2)) error stop 10_4

    select type (b1)
        type is (base(8))
            b1 = b2
        type is (child(8,1,*))
            select type (b2)
                type is (child(8,1,*))
                    b1 = b2
                class default
                    error stop 11_4
            end select
        class default
            error stop 12_4
    end select
end subroutine


program fclass017
use m
    class(base(8)), allocatable :: b1
    class (base(8)), pointer :: b2(:)

    allocate (child(8,1,20) :: b1, b2(3))

    b2(1) = b1

    call b2(1)%print

    select type (b2)
        type is (child(8,1,*))
            allocate (b2(1)%data(4), source=(/(1.2_8, 2.1_8), (3.2_8, 2.3_8), &
                        (4.5_8, 5.4_8), (6.3_8, 3.6_8)/))

            allocate (b2(2)%name, source='xlftest team')

            allocate (b2(3)%data(0:3), source=b2(1)%data)
            allocate (b2(3)%name, source=b2(2)%name)
        class default
            error stop 1_4
    end select

    b1 = b2(1)

    call b1%print

    b1 = b2(2)

    call b1%print

    b1 = b2(3)

    call b1%print
end
