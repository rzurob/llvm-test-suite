! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet009a.f
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
!*  DATE                       : 05/04/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : function return (binary defined operator)
!*
!*
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

        print *, 'r1; bounds: ', lbound(b%r1,1), ubound(b%r1,1)

        do i = lbound(b%r1,1), ubound(b%r1,1)
            write (*, '(f10.2)') b%r1(i)
        end do
    end subroutine

    subroutine printChild (b)
        class (child(8)), intent(in) :: b

        call b%base%print

        print *, 'i1; bounds: ', lbound(b%i1,1), ubound(b%i1,1)

        do i = lbound(b%i1,1), ubound(b%i1,1)
            write (*, '(i10)') b%i1(i)
        end do
    end subroutine

    subroutine finalizeChild (c)
        type (child(8)), intent(inout) :: c

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
            class (base(8)), intent(in) :: b1, b2
            class(base(8)), allocatable :: returnVal
        end function
    end interface

    class(base(8)), allocatable :: b1, b2, b3

    associate (x1 => (base(8)((/1.0, 3.2/)) + base(8)(null())), &
               x2 => base(8)(null()) + base(8)((/4.3, 5.2/)))
        call x1%print
        call x2%print
    end associate

    allocate (b1, source=base(8)((/1.5, 3.2/)))
    allocate (b2)

    allocate (b2%r1(0:2), source= (/1.1_8, 2.2_8, 3.3_8/))

    allocate (b3, source=b1+b2)

    call b3%print
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
