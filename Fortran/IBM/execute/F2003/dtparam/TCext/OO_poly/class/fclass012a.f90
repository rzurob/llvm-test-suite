! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass012a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/02/2005
!*
!*  DESCRIPTION                : CLASS keyword (select type construct and
!                               defined assignment)
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
        integer, kind         :: k1
        real(k1), allocatable :: data
    end type

    type, extends(base) :: child(k2,n1)    ! (8,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    interface assignment (=)
        elemental subroutine b1AssgnB2 (b1, b2)
        import base
            class (base(8)), intent(inout) :: b1
            class (base(8)), intent(in) :: b2
        end subroutine
    end interface

    contains

    subroutine printBaseArray (b)
        class (base(8)), intent(in) :: b(:)

        select type (b)
            type is (base(8))
                do i = 1, size(b)
                    if (allocated (b(i)%data)) write (*, '(f10.2)') b(i)%data
                end do
            type is (child(8,1,*))
                do i = 1, size(b)
                    if (allocated (b(i)%data)) then
                        write (*, '(f10.2, a, a)') b(i)%data, '; ', b(i)%name
                    else
                        write (*, '(a)') b(i)%name
                    end if
                end do
            class default
                error stop 10_4
        end select
    end subroutine
end module


elemental subroutine b1AssgnB2 (b1, b2)
use m, only: base, child
    class (base(8)), intent(out) :: b1
    class (base(8)), intent(in) :: b2

    if (.not. same_type_as (b1, b2)) return

    select type (b2)
        type is (base(8))
            select type (b1)
                type is (base(8))
                    b1 = b2
            end select
        type is (child(8,1,*))
            select type (b1)
                type is (child(8,1,*))
                    b1 = b2
            end select
    end select
end subroutine

program fclass012a
use m
    class (*), pointer :: x1(:)

    class (base(8)), allocatable :: b1(:), b2(:)

    allocate (child(8,1,20) :: x1(2))
    allocate (b1(0:1), source=(/child(8,1,20)(1.2_8, 'abc'), child(8,1,20)(2.1_8, 'xyz')/))

    allocate (b2(4), source=(/(base(8)(1.2_8*j), j=1, 3), base(8)(null())/))

    !! test assignment for child type
    select type (x1)
        class is (base(8))
            x1 = b1

            call printBaseArray(x1)
        class default
            error stop 1_4
    end select


    !! test assignment for base type
    deallocate (x1)

    allocate (base(8):: x1(size(b2)))

    select type (x1)
        class is (base(8))
            x1 = b2

            call printBaseArray(x1)
    end select
end
