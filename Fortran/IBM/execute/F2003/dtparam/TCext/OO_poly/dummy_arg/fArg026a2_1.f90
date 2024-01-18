! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg026a2_1.f
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

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine findMatchingID (b, func, id)
        class (base(4)), intent(in) :: b (10)
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

    class (base(4)), allocatable :: b2(:)
    type (child(4,1,20)) :: c1 (10)

    c1%id = (/(i,i=1,10)/)
    c1%name = 'c1'

    allocate (b2(10), source=c1)

    call findMatchingID (b2, matchID, 6)
end

logical function matchID (b, i, id)
use m
    type (base(4)), intent(in) :: b (10)
    integer*4, intent(in) :: i
    integer*4, intent(in) :: id

    matchID = (b(i)%id == id)
end function
