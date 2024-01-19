! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/04/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (procedure with implicit
!*                              interface used as actual argument)
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
    type base
        integer*4 :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine findMatchingID (b, func, id)
        class (base), intent(in) :: b (10)
        logical func
        integer*4, intent(in) :: id

        do i = 1, 10
            if (func(b, i, id)) print *, i
        end do
    end subroutine
end module

program fArg026a2_1
use m
    logical matchID
    external matchID

    class (base), allocatable :: b2(:)
    type (child) :: c1 (10)

    c1%id = (/(i,i=1,10)/)
    c1%name = 'c1'

    allocate (b2(10), source=c1)

    call findMatchingID (b2, matchID, 6)
end

logical function matchID (b, i, id)
use m
    type (base), intent(in) :: b (10)
    integer*4, intent(in) :: i
    integer*4, intent(in) :: id

    matchID = (b(i)%id == id)
end function
