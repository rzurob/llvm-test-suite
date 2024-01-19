! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet009.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/03/2005
!*
!*  DESCRIPTION                : poly function return (defined operator)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind         :: k1
        real(k1), allocatable :: r1

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2,n1)    ! (8,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    interface operator(-)
        module procedure minus
    end interface

    contains

    subroutine printBase (b)
        class (base(8)), intent(in) :: b

        if (allocated(b%r1)) then
            write (*, '(f10.2)') b%r1
        else
            print *, 'r1 not allocated'
        end if
    end subroutine

    subroutine printChild (b)
        class (child(8,1,*)), intent(in) :: b

        if (allocated(b%r1)) then
            write (*, '(f10.2, 2a)') b%r1, ', ', b%name
        else
            print *, '|, ', b%name
        end if
    end subroutine

    class(base(8)) function minus (b)
        class (base(8)), intent(in) :: b
        allocatable minus

        select type (b)
            type is (base(8))
                if (allocated (b%r1)) then
                    allocate (minus, source=base(8)(-b%r1))
                else
                    allocate (minus)
                end if
            type is (child(8,1,*))
                if (allocated (b%r1)) then
                    allocate (minus, source=child(8,1,20)(-b%r1, '-'//b%name))
                else
                    allocate (minus, source=child(8,1,20)(null(), '-'//b%name))
                end if
            class default
        end select
    end function
end module

program ffuncRet009
use m
    class (base(8)), pointer :: b1

    type(base(8)) :: b2

    allocate (b1)
    allocate (b1%r1, source= 2.5_8+3.1_8)

    associate (x1 => (- b1), x2 => -child(8,1,20)(1.5, 'test 01'))
        call x1%print
        call x2%print
    end associate

    !! 2nd test
    deallocate (b1)

    allocate (b1, source=-child(8,1,20)(-3.2, 'test 02'))

    call b1%print

    deallocate (b1%r1)

    b2 = -b1

    call b1%print
    call b2%print
end
