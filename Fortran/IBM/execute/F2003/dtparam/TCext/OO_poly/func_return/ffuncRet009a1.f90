! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet009a1.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/04/2005
!*
!*  DESCRIPTION                : poly function return (binary defined operator)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind         :: k1
        real(k1), allocatable :: r1(:)

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child    ! (8)
        integer(k1), pointer :: i1(:) => null()

        contains

        procedure :: print => printChild
        final :: finalizeChild
    end type

    contains

    subroutine printBase (b)
        class(base(8)), intent(in) :: b

        if (allocated(b%r1)) then
            print *, 'r1; bounds: ', lbound(b%r1,1), ubound(b%r1,1)

            do i = lbound(b%r1,1), ubound(b%r1,1)
                write (*, '(f10.2)') b%r1(i)
            end do
        end if
    end subroutine

    subroutine printChild (b)
        class (child(8)), intent(in) :: b

        call b%base%print

        if (associated(b%i1)) then
            print *, 'i1; bounds: ', lbound(b%i1,1), ubound(b%i1,1)

            do i = lbound(b%i1,1), ubound(b%i1,1)
                write (*, '(i10)') b%i1(i)
            end do
        end if
    end subroutine

    subroutine finalizeChild (c)
        type (child(8)), intent(inout) :: c

        if (associated (c%i1)) then
            print *, 'deallocating i1'
            deallocate (c%i1)
        end if
    end subroutine
end module

program ffuncRet009a1
use m
    interface operator(+)
        function addB1B2 (b1, b2) result (returnVal)
        use m
            class (base(8)), intent(in) :: b1, b2
            class(base(8)), allocatable :: returnVal
        end function
    end interface

    class(base(8)), allocatable :: b1, b2, b3

    allocate (b1, source=child(8)((/2.1, 5.2, 1.4/), null()))

    allocate (b2, source=child(8)((/1.1/), null()))

    associate (x => b1 + b1 + b2)
        call x%print
    end associate

    select type (b2)
        type is (child(8))
            allocate (b2%i1(0:1), source= (/10_8, -1_8/))
        class default
            error stop 1_4
    end select

    associate (x => b2 + b1)
        call x%print

        select type (x)
            type is (child(8))
                allocate (b3, source=child(8)(x%r1, null()))

                select type (b3)
                    type is (child(8))
                        allocate (b3%i1(size(x%i1)), source=x%i1)
                    class default
                        error stop 3_4
                end select
            class default
                error stop 2_4
        end select
    end associate

    call b3%print

    select type (x => b3+b3)
        type is (child(8))
            call x%print
        class default
            error stop 5_4
    end select

    print *, 'end'
end


function addB1B2 (b1, b2) result (returnVal)
use m
    class (base(8)), intent(in) :: b1, b2
    class(base(8)), allocatable :: returnVal

    integer(8), pointer :: iArray(:)

    nullify (iArray)

    if (.not. same_type_as (b1, b2)) error stop 10_4

    select type (b1)
        type is (base(8))
            allocate (returnVal)

            call addR1()
        type is (child(8))
            select type (b2)
                type is (child(8))
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

                    allocate (child(8) :: returnVal)

                    call addR1()

                    select type (returnVal)
                        type is (child(8))
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
