!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/10/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Still the linkedList but with dataType of
!                               polymorphic. similar to dtparamDefVal004.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k = 8
        integer, len :: n = 3000

        complex(k) :: cx(n)
    end type

    type, extends(base) :: child(l)
        integer, len :: l = 20

        character(l) :: name
        integer(k) :: ids(n)
    end type
end module

module listMod
use m
    type node
        private
        class(base), allocatable :: data
        type (node), pointer :: next => null()
    end type

    type(node), pointer :: iterator => null()

    contains

    subroutine pushBack (n, b)
        class(node), intent(inout), target :: n
        class(base), intent(in) :: b

        type (node), pointer :: temp

        if (.not. allocated (n%data)) then !<-- empty list
            allocate (n%data, source=b)
        else
            temp => n

            do while (associated(temp) .and. associated(temp%next))
                temp => temp%next
            end do

            allocate (temp%next, source=node(b))
        end if
    end subroutine

    subroutine begin(n)
        type(node), intent(in), target :: n

        iterator => n
    end subroutine

    logical function hasMore ()
        hasMore = associated(iterator%next)
    end function

    subroutine next()
        iterator => iterator%next
    end subroutine

    class(base) function getVal ()
        allocatable :: getVal

        allocate (getVal, source=iterator%data)
    end function
end module

program dtparamDefVal005
use m
use listMod
    class(node), pointer :: list
    class(base), allocatable :: b1
    class(base), pointer :: b2

    logical(4), external :: precision_x6

    allocate(list)

    nullify (b2)

    do i = 1, 200
        if (mod(i,2) == 0) then
            allocate (b1, source=base(cmplx(log((/(i*1.0d4+j, j=1,3000)/)), &
                log10((/(i*1.0d4+j, j=1,3000)/)), 8)))
        else
            allocate (b2, source=child(cmplx((/(i*1.0d4+j, j=1,3000)/), &
                (/(i*1.0d4+j, j=1,3000)/), 8), 'b2 allocated', &
                (/(i*10000+j, j=1,3000)/)))
        end if

        if (allocated(b1)) then
            call pushBack (list, b1)

            deallocate (b1)
        else
            call pushBack (list, b2)

            deallocate (b2)
        end if
    end do

    !! verify results
    call begin(list)

    i = 1

10  select type (x => getVal())
        type is (base(8,*))
            if (mod(i, 2) /= 0) error stop 2_4

            do j = 1, 3000
                if (.not. precision_x6 (x%cx(j), (log(i*1.0d4+j), &
                    log10(i*1.0d4+j)))) error stop 3_4
            end do

        class is (child(8,*,*))
            if (mod(i, 2) /= 1) error stop 4_4

            do j = 1, 3000
                if (.not. precision_x6 (x%cx(j), (i*1.0d4+j, i*1.0d4+j))) &
                    error stop 5_4
            end do

            if (x%name /= 'b2 allocated') error stop 6_4
            if (any(x%ids /= (/(i*10000+j, j=1,3000)/))) error stop 7_4

        class default
            error stop 1_4
    end select

    if (hasMore()) then
        i = i + 1

        call next()
        goto 10
    end if

    if (i /= 200) error stop 10_4
end
