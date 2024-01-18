! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg035a.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/17/2005
!*
!*  DESCRIPTION                : argument association (nested function results
!                               as the actual-arg)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), pointer :: data(:) => null()
    end type

    type, extends(base) :: child(k2)    ! (4,20,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name
    end type

    contains

    class(base(4,:)) function copyData (b)
        class (base(4,:)), allocatable, intent(in) :: b(:)
        allocatable copyData(:)

        if (allocated (b)) allocate (copyData(size(b)), source=b)
    end function
end module

program fArg035a
use m
    class(base(4,:)), allocatable :: b1(:)

    allocate (b1(3), source=child(4,20,1)(null(), 'test xlf'))

    allocate (b1(1)%data(2), source=(/1, 2/))
    allocate (b1(2)%data(2), source=(1.5_8, 2.5_8))
    allocate (b1(3)%data(3), source=(/'t1', 't2', 't3'/))

    associate (x => copyData (copyData(copyData(copyData(b1)))))
        if (size(x) /= 3)   error stop 1_4

        if ((.not. associated (x(1)%data, b1(1)%data)) .or. &
            (.not. associated (x(2)%data, b1(2)%data)) .or. &
            (.not. associated (x(3)%data, b1(3)%data))) error stop 2_4


        call printX (x(1)%data)
        call printX (x(2)%data)
        call printX (x(3)%data)

        select type (y => x)
            type is (child(4,*,1))
                print *, y%name
            class default
                error stop 10_4
        end select
    end associate

    contains

    subroutine printX (x)
        class(*), intent(in) :: x(:)

        select type (y => x)
            type is (integer)
                print *, y
            type is (complex(8))
                write (*, '("(", f10.2, ",", f10.2, " )")') y
            type is (character(*))
                write (*, '(5(a,1x))') y
            class default
                print *, 'unknown type'
        end select
    end subroutine
end
