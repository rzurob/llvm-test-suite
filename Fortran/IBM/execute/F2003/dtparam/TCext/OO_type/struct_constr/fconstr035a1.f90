! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr035a1.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/21/2005
!*
!*  DESCRIPTION                : structure constructor (structure constructor
!                               overwritten by generic name)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        double precision, allocatable :: data(:)
    end type

    type, extends(base) :: child(k2,n2)    ! (4,20,1,20)
        integer, kind             :: k2
        integer, len              :: n2
        character(kind=k2,len=n2) :: name
    end type

    type container(k3,n3)    ! (4,20)
        integer, kind                   :: k3
        integer, len                    :: n3
        class(base(k3,n3)), allocatable :: data(:)
    end type

    interface container
        module procedure createContainer
    end interface


    contains

    !! this function always returns a container type with data starts
    !from 10 as the lower bound
    type (container(4,20)) function createContainer (b1)
        class (base(4,*)), intent(in) :: b1(10:)

        allocate (createContainer%data(lbound(b1,1):ubound(b1,1)), &
                    source= b1)
    end function
end module

program fconstr035a1
use m
    !! first call
    associate (x => container((/child(4,20,1,20)((/1.0, 2.0/), 'test1'), &
                                child(4,20,1,20)((/2.5, 3.5/), 'test2')/)))

        if (.not. allocated (x%data)) error stop 1_4

        if (lbound(x%data, 1) /= 10) error stop 2_4
        if (ubound(x%data, 1) /= 11) error stop 3_4

        select type (y => x%data)
            type is (child(4,*,1,*))
                if (any(y%name /= (/'test1', 'test2'/))) error stop 4_4

                write (*, '(4f10.2)') y(10)%data, y(11)%data
            class default
                error stop 6_4
        end select
    end associate


    !! second call
    associate (x => container (createBaseAlloc ((/10.2_8, 2.1_8/), 2)))
        if (.not. allocated (x%data)) error stop 7_4

        if (lbound (x%data, 1) /= 10) error stop 8_4
        if (ubound(x%data, 1) /= 11) error stop 9_4

        select type (y => x%data)
            type is (base(4,*))
                write (*, '(4f10.2)') y(10)%data, y(11)%data
            class default
                error stop 10_4
        end select
    end associate

    contains

    class (base(4,20)) function createBaseAlloc (d1, isize, name)
        double precision, intent(in) :: d1(:)
        integer, intent(in) :: isize
        character(*), intent(in), optional :: name (isize)

        allocatable createBaseAlloc(:)

        if (present(name)) then
            allocate (createBaseAlloc(isize), source=(/(child(4,20,1,20)(d1, name(j)), &
                        j = 1, isize)/))
        else
            allocate (createBaseAlloc(isize), source=base(4,20)(d1))
        end if
    end function
end
