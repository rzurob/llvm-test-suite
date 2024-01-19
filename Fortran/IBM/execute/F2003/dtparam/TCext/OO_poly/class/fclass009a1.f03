! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass009a1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (operator(+) defined for derived
!                               type; use poly function return; test scalars)
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
    type base(k1)    ! (4)
        integer, kind            :: k1
        integer(k1), allocatable :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    interface operator(+)
        class (base(4)) function addB1B2 (b1, b2)
        import base
            class (base(4)), intent(in) :: b1, b2
            allocatable :: addB1B2
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        if (allocated(b%id)) then
            print *, b%id
        else
            print *, 'id not allocated'
        end if
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        if (allocated(b%id)) then
            print *, b%id, b%name
        else
            print *, 'id not allocated; ', b%name
        end if
    end subroutine
end module

program fclass009a1
use m
    class (base(4)), allocatable :: b1

    allocate (b1)

    associate (x => b1 + base(4) (1))
        call x%print
    end associate

    associate (x => child(4,1,20)(10, 'xlftest') + child(4,1,20)(20, 'team'))
        call x%print
    end associate

    deallocate (b1)
    allocate (b1, source=child(4,1,20)(100, 'compiler'))

    associate (x => b1 + child(4,1,20)(-10, 'test') + b1)
        call x%print
    end associate
end


class (base(4)) function addB1B2 (b1, b2)
use m, only: base, child
    class (base(4)), intent(in) :: b1, b2
    allocatable :: addB1B2

    integer id1, id2
    character(20) :: localName

    id1 = 0
    id2 = 0

    if (.not. same_type_as (b1, b2)) error stop 1_4

    if (allocated (b1%id)) id1 = b1%id
    if (allocated (b2%id)) id2 = b2%id

    select type (b1)
        type is (base(4))
            allocate (addB1B2, source=base(4)(id1+id2))
        type is (child(4,1,*))
            select type (b2)
                type is (child(4,1,*))
                    localName = trim(b1%name) // ' ' // trim(b2%name)

                    allocate (addB1B2, source=child(4,1,20)(id1+id2, localName))
                class default
                    error stop 8_4
            end select
        class default
            error stop 10_4
    end select
end function
