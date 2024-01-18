! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass012.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/02/2005
!*
!*  DESCRIPTION                : CLASS keyword (select type construct and
!                               defined operator)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind            :: k1
        integer(k1), allocatable :: id
    end type

    type, extends(base) :: child(k2,n1)    ! (8,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    interface operator(==)
        elemental logical function b1B2equal (b1, b2)
        import base
            class (base(8)), intent(in) :: b1, b2
        end function
    end interface
end module


elemental logical function b1B2equal (b1, b2)
use m, only: base, child
    class (base(8)), intent(in) :: b1, b2

    if (.not. same_type_as (b1, b2)) then
        b1B2equal = .false.
        return
    end if

    if (allocated(b1%id) .neqv. allocated (b2%id)) then
        b1B2equal = .false.
        return
    end if

    select type (b1)
        type is (base(8))
            if (allocated(b1%id)) then
                b1B2equal = (b1%id == b2%id)
            end if
        type is (child(8,1,*))
            select type (b2)
                type is (child(8,1,*))
                    if (allocated (b1%id)) then
                        b1B2equal = ((b1%id == b2%id) .and. (b1%name == b2%name))
                    else
                        b1B2equal = (b1%name == b2%name)
                end if

                class default
                    b1B2equal = .false.
            end select
        class default
            b1B2equal = .false.
    end select
end function


program fclass012
use m
    class (*), allocatable :: x1(:)

    class (base(8)), allocatable :: b2(:), b3(:)

    allocate (x1(2), source=(/child(8,1,20) (1_8, 'x1'), child(8,1,20)(2_8, 'x2')/))
    allocate (b2(2), source=(/child(8,1,20) (1_8, 'x1'), child(8,1,20)(2_8, 'x2')/))

    allocate (b3(0:2), source = (/child(8,1,20)(null(), 'x1'), child(8,1,20)(2_8, 'b3'), &
                        child(8,1,20) (1_8, 'x1')/))

    select type (x1)
        class is (base(8))
            if (.not. all(x1 == b2)) error stop 1_4
            if (.not. all(b2 .eq. x1)) error stop 2_4

            if (any (x1 == b3(0:1))) error stop 3_4

            if (.not. (x1(1) == b3(2))) error stop 4_4
        class default
            error stop 10_4
    end select
end
