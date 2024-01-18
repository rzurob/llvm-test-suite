! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/11/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Deferred type-parameters in
!                               declaration-type-spec: dummy-arg.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type intArray (n)
        integer, len :: n = 10

        integer :: id(n) = -1
    end type

    contains

    subroutine printIntArray (ia)
        class(intArray(:)), allocatable, intent(in) :: ia(:)

        if (.not. allocated(ia)) then
            print *, 'actual argument is unallocated'
            return
        else
            do i = lbound(ia,1), ubound(ia, 1)
                print *, ia(i)%id
            end do
        end if
    end subroutine

    subroutine setArray (ia, ids)
        class (intArray(:)), allocatable, intent(out) :: ia(:)
        integer, intent(in) :: ids(:,:)

        allocate (intArray(size(ids, 1)) :: ia(size(ids, 2)))

        do i = 1, size(ia)
            ia(i)%id = ids(:, i)
        end do
    end subroutine
end module

program deferdparamDTSpec008
use m
    integer ids1(10, 20), ids2 (100, 200)
    class (intArray(:)), allocatable :: ia1(:)

    ids1 = reshape((/(i, i=1, 200)/), (/10, 20/))
    ids2 = reshape((/(i, i=20000, 1, -1)/), (/100, 200/))

    call printIntArray (ia1)

    call setArray (ia1, ids1)

    print *, 'test 2'

    call printIntArray (ia1)

    call setArray (ia1, ids2(30::2, 5:7:2))

    print *, 'test 3'

    call printIntArray (ia1)

    deallocate (ia1)

    allocate (intArray(3):: ia1(3))

    ia1(1)%id = ids2(3, (/2, 5, 1/))
    ia1(2)%id = ids2((/100, 75, 99/), 150)

    call printIntArray (ia1)
end
