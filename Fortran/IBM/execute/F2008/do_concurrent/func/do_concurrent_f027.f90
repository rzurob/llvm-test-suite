!*  ===================================================================
!*
!*  DATE                       : 08/21/2015
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*      Check if a polymorhic subroutine could be used inside a do concurrent
!*    poly function return (poly function results in reshape() and used as
!*    the actual arg)
!*    based on OO_poly/func_return/ffuncRet501.f by Jim Xia
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(selected_int_kind (2)) :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(15) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    pure subroutine printBase (b)
        class (base), intent(in) :: b
    end subroutine

    pure subroutine printChild (b)
        class (child), intent(in) :: b
    end subroutine

    class (base) function produceBaseAlloc (id, name, nsize)
        allocatable produceBaseAlloc(:)
        integer(selected_int_kind(2)), intent(in) :: id
        integer(4), intent(in) :: nsize
        character(*), optional, intent(in) :: name

        if (present(name)) then     ! allocate of type child
            allocate (produceBaseAlloc(nsize), &
                    source=(/(child(i,name), i=ID, id+nsize)/))
        else
            allocate (produceBaseAlloc(nsize), &
                    source=(/(base(i), i=id, id+nsize)/))
        end if
    end function

    subroutine printData (d)
        class (base), intent(in) :: d (:,:)

        do concurrent (j = 1:size(d,2))
          do concurrent (i = 1:size(d,1))
            call d(i,j)%print
            select type (d)
              type is (base)
                print *, "type is base"
              type is (child)
                print *, "type is child"
              class default
                error stop 3
            end select
          end do
        end do
    end subroutine
end module


program ffuncRet501
use m
    call printData (reshape (produceBaseAlloc (1_1, 'test', 4), (/2,2/)))
    call printData (reshape (produceBaseAlloc (1_1, nsize=4), (/2,2/)))
end
