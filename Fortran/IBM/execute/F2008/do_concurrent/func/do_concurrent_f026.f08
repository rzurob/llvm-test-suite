! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/17/2015
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : poly function return used inside a do
!*                               concurrent construct
!*                               based on OO_poly/func_return/ffuncRet501.f
!*                               by Jim Xia
!*
!*  KEYWORD(S)                 : DO CONCURRENT, F2008
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

    pure integer*4 function printBase (b)
        class (base), intent(in) :: b

        printBase=22
    end function

    pure integer*4 function printChild (b)
        class (child), intent(in) :: b

        printChild=33
    end function

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
        integer*4 :: tmp =0

        do concurrent (j = 1:size(d,2), i = 1:size(d,1))
            tmp=d(i,j)%print()

            select type (d)
                type is (base)
                    if(tmp .ne. 22) error stop 1
                type is (child)
                    if(tmp .ne. 33) error stop 2
                class default
                    error stop 3
            end select

        end do
    end subroutine

end module


program ffuncRet501
use m
    call printData (reshape (produceBaseAlloc (1_1, 'test', 4), (/2,2/)))
    call printData (reshape (produceBaseAlloc (1_1, nsize=4), (/2,2/)))
end
