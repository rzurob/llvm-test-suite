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
!*  DATE                       : 01/31/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : argument association (unlimited poly dummy-arg
!                               associated with actual-arg of array constructor
!                               with poly-entities)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, allocatable :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        if (allocated (b%id)) then
            print *, b%id
        else
            print *, 'id not allocated'
        end if
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        if (allocated (b%id)) then
            print *, b%id, b%name
        else
            print *, 'id not allocated;', b%name
        end if
    end subroutine

    subroutine printX (x)
        class (*), intent(in) :: x(:)

        select type (x)
            class is (base)
                do i = 1, size(x)
                    call x(i)%print
                end do
            class default
                print *, 'other type'
        end select
    end subroutine
end module


program fArg520a
use m
    class (base), allocatable :: b1(:,:)

    class (*), allocatable :: x1(:,:)

    allocate (b1(2,2), source=reshape ((/child (1, 'b1 1'), child(2, 'b1 2'), &
                child (3, 'b1 3'), child(4, 'b1 4')/), (/2,2/)))

    call printX ((/b1(1,:), b1(2,:)/))


    allocate (x1(2,2), source=reshape ((/child (1, 'x1 1'), child(2, 'x1 2'), &
                child (3, 'x1 3'), child(4, 'x1 4')/), (/2,2/)))

    call printX ((/x1(2:1:-1,2), x1(2:1:-1,1)/))
end
