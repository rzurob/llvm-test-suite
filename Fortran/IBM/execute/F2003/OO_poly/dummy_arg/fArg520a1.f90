!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/31/2005
!*
!*  DESCRIPTION                : argument association (poly-function return
!                               results to be associated with unlimited poly
!                               dummy argument array)
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
        integer(8) id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character (18) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) ::b

        print *, b%id, b%name
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


program fArg520a1
use m
    class (base), allocatable :: b1(:,:)

    allocate (b1(2,2), source=reshape ((/child(1, 'b1 1'), child(2, 'b1 2'), &
                child(3, 'b1 3'), child(4, 'b1 4')/), (/2,2/)))

    call printX (reshape ((/b1(2:1:-1,2), b1(2:1:-1,1)/), (/3/)))

    call printX (reshape (b1, (/3/)))
end
