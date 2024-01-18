!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/13/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Deferred type-parameters:
!                               defined/undefined during argument association;
!                               use pointer entities; test scalar.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        integer :: ids(n)
    end type

    type, extends(base) :: child
        complex :: cx(n)
    end type

    interface
        subroutine setBase (b, ids, cx)
        import
            class(base(:)), pointer, intent(inout) :: b
            integer, intent(in) :: ids(:)
            complex, intent(in), optional :: cx(size(ids))
        end subroutine
    end interface

    contains

    subroutine printBasePtr (b)
        class(base(:)), intent(in), pointer :: b

        if (.not. associated(b)) then
            print *, 'disassociated pointer'
        else
            select type (b)
                type is (base(*))
                    print *, b%ids

                type is (child(*))
                    print *, b%ids
                    write(*, '(3("(", f10.2, ",", f10.2,") "))') b%cx
            end select
        end if
    end subroutine
end module

program deferdparamDTSpec017
use m
    class(base(:)), pointer :: b1

    complex, allocatable :: cx1(:)

    nullify (b1)

    call printBasePtr (b1)

    allocate (base(10) :: b1)

    b1%ids = (/(i, i=1, 10)/)

    call printBasePtr (b1)

    allocate (cx1(10), source=(/(cmplx(i,i+1), i=1, 10)/))

    call setBase (b1, (/(11-i, i=1, 10)/), cx1)

    call printBasePtr (b1)

    call setBase (b1, (/3,2,1/))

    call printBasePtr (b1)
end


subroutine setBase (b, ids, cx)
use m, only: base, child
    class(base(:)), pointer, intent(inout) :: b
    integer, intent(in) :: ids(:)
    complex, intent(in), optional :: cx(size(ids))

    if (associated(b)) deallocate (b)

    if (present(cx)) then
        allocate (child(size(ids)):: b)
    else
        allocate (base(size(ids)) :: b)
    end if

    b%ids = ids

    select type (b)
        type is (child(*))
            b%cx = cx
    end select
end subroutine
