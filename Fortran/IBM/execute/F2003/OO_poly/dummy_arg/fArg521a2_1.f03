! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/02/2005
!*
!*  DESCRIPTION                : argument association (a work-around for
!                               fArg521a2.f with all the polymorphism removed
!                               for the function results)
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
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    contains

    type (child) function produceBasePtr (id, name)
        pointer produceBasePtr (:,:)
        integer, intent(in) :: id (:,:)
        character(*), intent(in) :: name(:,:)

        allocate (child:: produceBasePtr(size(id,1),size(id,2)))

        !! set up values for id
        do i = 1, size(id,1)
            do j = 1, size(id,2)
                allocate (produceBasePtr(i,j)%id, source=id(i,j))
            end do
        end do

        produceBasePtr%name = name
    end function

    subroutine printVal (b)
        class (base), intent(in) :: b(:)

        do i = 1, size(b)
            select type (x => b(i))
                type is (child)
                    if (allocated (x%id)) then
                        print *, x%id, x%name
                    else
                        print *, 'id not allocated; ', x%name
                    end if
                class default
                    error stop 10_4
            end select
        end do
    end subroutine
end module

program fArg521a2_1
use m
    integer ids(2,2)
    character(20) names(2,2)

    ids = reshape ((/11, 21, 12, 22/), (/2,2/))

    names = reshape ((/'test 11', 'test 21', 'test 12', 'test 22'/), (/2,2/))

    call printVal ((/produceBasePtr(ids, names)/))
end