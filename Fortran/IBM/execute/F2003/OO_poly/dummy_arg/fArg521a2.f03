! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/02/2005
!*
!*  DESCRIPTION                : argument association (poly function return
!                               pointer array used in array constructor, which
!                               is associated with poly dummy-arg)
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

    class (base) function produceBasePtr (id, name)
        pointer produceBasePtr (:,:)
        integer, intent(in) :: id (:,:)
        character(*), intent(in), optional :: name(:,:)

        if (present(name)) then
            if (any(shape(name) /= shape(id))) error stop 1_4

            allocate (child:: produceBasePtr(size(id,1),size(id,2)))

            select type (produceBasePtr)
                type is (child)
                    produceBasePtr%name = name
                class default
                    error stop 2_4
            end select
        else
            allocate (produceBasePtr(size(id,1),size(id,2)))
        end if

        !! set up values for id
        do i = 1, size(id,1)
            do j = 1, size(id,2)
                allocate (produceBasePtr(i,j)%id, source=id(i,j))
            end do
        end do
    end function

    subroutine printVal (b)
        class (base), intent(in) :: b(:)

        do i = 1, size(b)
            select type (x => b(i))
                type is (base)
                    if (allocated (x%id)) then
                        print *, x%id
                    end if
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

program fArg521a2
use m
    integer ids(2,2)
    character(20) names(2,2)

    ids = reshape ((/11, 21, 12, 22/), (/2,2/))

    names = reshape ((/'test 11', 'test 21', 'test 12', 'test 22'/), (/2,2/))

    call printVal ((/produceBasePtr(ids, names)/))
end
