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
!*  DATE                       : 01/11/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Deferred type-parameters in
!                               declaration-type-spec: zero-size array and array
!                               components.
!*
!*
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

program deferdparamDTSpec008a
use m
    integer ids1(10, 20), ids2 (100, 200)
    class (intArray(:)), allocatable :: ia1(:)

    ids1 = reshape((/(i, i=1, 200)/), (/10, 20/))
    ids2 = reshape((/(i, i=20000, 1, -1)/), (/100, 200/))

    call printIntArray (ia1)

    call setArray (ia1, ids1(10:1, :))   !<-- zero-size array component

    print *, 'test 2'

    call printIntArray (ia1)

    call setArray (ia1, ids2(30::2, 5:7:-2))    !<-- zero-size array ia1

    print *, 'test 3'

    call printIntArray (ia1)

    deallocate (ia1)

    allocate (intArray(-3):: ia1(3))    !<-- zero-size array component

    print *, 'test 4'

    call printIntArray (ia1)
end
