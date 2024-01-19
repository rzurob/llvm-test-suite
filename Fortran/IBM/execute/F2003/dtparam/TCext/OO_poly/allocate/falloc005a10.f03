! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc005a10.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (function reference as source-expr in
!                               ALLOCATE statement)
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

    interface makeData
        function produceBaseObj (id)
            import base
            type (base(4)) produceBaseObj
            integer(4), intent(in) :: id
        end function

        function produceChildObj (id, name)
            import child
            type (child(4,1,20)) produceChildObj
            integer(4), intent(in) :: id
            character(*), intent(in) :: name
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
end module

program falloc005a10
use m
    class (base(4)), allocatable :: b1, b2(:)

    type (child(4,1,20)), pointer :: c1

    allocate (b1, source=makeData (10))

    allocate (b2(3:5), source=makeData (20, 'b2_array_of_3'))

    allocate (c1, source=makeData (30, 'c1'))

    call b1%print

    call c1%print

    call b2(3)%print
    call b2(4)%print
    call b2(5)%print
end

type (base(4)) function produceBaseObj (id)
use m, only: base
    integer(4), intent(in) :: id

    produceBaseObj%id = id
end function


type (child(4,1,20)) function produceChildObj (id, name)
use m, only: child
    integer(4), intent(in) :: id
    character(*), intent(in) :: name

    produceChildObj%id = id
    produceChildObj%name = name
end function
