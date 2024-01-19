! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/25/2005
!*
!*  DESCRIPTION                : CLASS keyword (defined assignment,
!                               finalization, structure constructor overriding
!                               and forall construct; use elemental procedures)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        class(*), pointer :: data(:) => null()

        contains

        final :: finalizeBase
    end type

    interface base
        pure type (base) function produceBaseObj (x)
        import base
            class (*), intent(in) :: x(:)
        end function
    end interface

    interface assignment(=)
        module procedure assgnB1FromB2
    end interface

    contains

    elemental subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        !! try to clean up
        if (associated(b%data)) deallocate(b%data, stat=i1)
    end subroutine

    elemental subroutine assgnB1FromB2 (b1, b2)
        class (base), intent(out) :: b1
        class (base), intent(in) :: b2

        if (associated (b2%data)) then
            allocate (b1%data(size(b2%data)), source=b2%data)
        end if
    end subroutine
end module


program fclass016
use m
    class (base), allocatable :: b1(:)

    allocate (b1(10))

    forall (i=1:10) b1(i) = base((/(j, j = 1, i)/))

    !! verify b1
    call printBaseArray (b1)

    !! assign again
    b1(6:) = b1(5:1:-1)

    print *, new_line('a'), 'test2', new_line('a')
    !! verify b1
    call printBaseArray (b1)

    contains

    subroutine printBaseArray (b)
        class (base), intent(in) :: b(:)

        do i = 1, size(b)
            print *, 'element', i

            select type (x => b(i)%data)
                type is (integer)
                    print *, x
                class default
                    error stop 10_4
            end select
        end do
    end subroutine
end


pure type (base) function produceBaseObj (x)
use m, only : base
    class (*), intent(in) :: x(:)

    allocate (produceBaseObj%data(size(x)), source=x)
end function
