! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/04/2005
!*
!*  DESCRIPTION                : function return (binary defined operator)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8), allocatable :: r1(:)

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        integer(8), pointer :: i1(:) => null()

        contains

        procedure :: print => printChild
        final :: finalizeChild
    end type

    contains

    subroutine printBase (b)
        class(base), intent(in) :: b

        print *, 'r1; bounds: ', lbound(b%r1,1), ubound(b%r1,1)

        do i = lbound(b%r1,1), ubound(b%r1,1)
            write (*, '(f10.2)') b%r1(i)
        end do
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        call b%base%print

        print *, 'i1; bounds: ', lbound(b%i1,1), ubound(b%i1,1)

        do i = lbound(b%i1,1), ubound(b%i1,1)
            write (*, '(i10)') b%i1(i)
        end do
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(inout) :: c

        if (associated (c%i1)) then
            print *, 'deallocating i1'
            deallocate (c%i1)
        end if
    end subroutine
end module

program ffuncRet009a
use m
    interface operator(+)
        function addB1B2 (b1, b2) result (returnVal)
        use m
            class (base), intent(in) :: b1, b2
            class(base), allocatable :: returnVal
        end function
    end interface

    class(base), allocatable :: b1, b2, b3

    associate (x1 => (base((/1.0, 3.2/)) + base(null())), &
               x2 => base(null()) + base((/4.3, 5.2/)))
        call x1%print
        call x2%print
    end associate

    allocate (b1, source=base((/1.5, 3.2/)))
    allocate (b2)

    allocate (b2%r1(0:2), source= (/1.1_8, 2.2_8, 3.3_8/))

    allocate (b3, source=b1+b2)

    call b3%print
end


function addB1B2 (b1, b2) result (returnVal)
use m
    class (base), intent(in) :: b1, b2
    class(base), allocatable :: returnVal

    integer(8), pointer :: iArray(:)

    nullify (iArray)

    if (.not. same_type_as (b1, b2)) error stop 10_4

    select type (b1)
        type is (base)
            allocate (returnVal)

            call addR1()
        type is (child)
            select type (b2)
                type is (child)
                    if (associated (b1%i1)) then    !! if b1%i1 associated
                        if (associated (b2%i1)) then
                            allocate (iArray(size(b1%i1)+size(b2%i1)), &
                                    source=(/b1%i1, b2%i1/))
                        else
                            allocate (iArray(size(b1%i1)), source=b1%i1)
                        end if
                    else                            !! if b1%i1 not associated
                        if (associated (b2%i1)) then
                            allocate (iArray(size(b2%i1)), source=b2%i1)
                        end if
                    end if

                    allocate (child :: returnVal)

                    call addR1()

                    select type (returnVal)
                        type is (child)
                            returnVal%i1 => iArray
                        class default
                            error stop 14_4
                    end select

                class default
                    error stop 11_4
            end select

        class default
            error stop 12_4
    end select

    contains

    subroutine addR1
        if (allocated(b1%r1)) then      !! if b1%r1 allocated
            if (allocated(b2%r1)) then
                allocate (returnVal%r1(size(b1%r1)+size(b2%r1)), source= &
                            (/b1%r1, b2%r1/))
            else
                allocate (returnVal%r1(size(b1%r1)), source= b1%r1)
            end if
        else                            !! if b1%r1 not allocated
            if (allocated(b2%r1)) then
                allocate (returnVal%r1(size(b2%r1)), source=b2%r1)
            end if
        end if
    end subroutine
end function
