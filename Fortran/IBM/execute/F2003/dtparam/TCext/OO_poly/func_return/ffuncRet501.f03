! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet501.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : poly function return (poly function results in
!                               reshape() and used as the actual arg)
!*
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        integer(selected_int_kind (2)) :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n2)    ! (4,20,1,15)
        integer, kind             :: k2
        integer, len              :: n2
        character(kind=k2,len=n2) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4,*)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,*,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    class (base(4,:)) function produceBaseAlloc (id, name, nsize)
        allocatable produceBaseAlloc(:)
        integer(selected_int_kind(2)), intent(in) :: id
        integer(4), intent(in) :: nsize
        character(*), optional, intent(in) :: name

        if (present(name)) then     ! allocate of type child(4,20,1,15)
            allocate (produceBaseAlloc(nsize), &
                    source=(/(child(4,20,1,15)(i,name), i=ID, id+nsize)/))
        else
            allocate (produceBaseAlloc(nsize), &
                    source=(/(base(4,20)(i), i=id, id+nsize)/))
        end if
    end function

    subroutine printData (d)
        class (base(4,*)), intent(in) :: d (:,:)

        do j = 1, size(d,2)
            do i = 1, size(d,1)
                call d(i,j)%print
            end do
        end do
    end subroutine
end module


program ffuncRet501
use m
    call printData (reshape (produceBaseAlloc (1_1, 'test', 4), (/2,2/)))
end
