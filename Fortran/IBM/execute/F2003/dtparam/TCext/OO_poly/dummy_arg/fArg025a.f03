! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg025a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (dummy procedure in
!                               argument association)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    interface
        function findAlg (b, id)
        import base
            class (base(4)), pointer :: findAlg (:)
            class (base(4)), intent(in) :: b (:)
            integer*4, intent(in) :: id
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printMatches (b, func, id)
        class (base(4)), intent (in) :: b(:)
        procedure (findAlg) func
        integer*4, intent(in) :: id

        class (base(4)), pointer :: temp(:)

        temp => func (b, id)

        if (associated (temp)) then
            do i = lbound(temp,1), ubound(temp, 1)
                call temp(i)%print
            end do

            deallocate (temp)
        else
            print *, 'there is no data with matching ID'
        end if
    end subroutine
end module

program fArg025a
use m
    procedure (findAlg) :: find1

    type(child(4,1,20)) :: c1 (10)

    c1 = (/(child(4,1,20)(mod(i,4), 'c1_'//char(ichar('0')+i-1)), i=1,10)/)

    call printMatches (c1, find1, 1)

    call printMatches (c1%base, find1, 3)

    call printMatches (c1(::3), find1, 0)

    call printMatches (c1((/1,2,10/)), find1, 3)
end


function find1 (b, id)
use m, only: base, child
    class (base(4)), pointer :: find1 (:)
    class (base(4)), intent(in) :: b (:)
    integer*4, intent(in) :: id

    integer*4 matchSize, indexs(size(b))

    nullify (find1)

    matchSize = 0
    indexs = 0

    do i = 1, size(b)
        if (b(i)%id == id) then    ! then we find a match based on id
            matchSize = matchSize + 1
            indexs(matchSize) = i
        end if
    end do

    if (matchSize /= 0) then
        allocate (find1(matchSize), source=b(indexs(1:matchSize)))
    end if
end function