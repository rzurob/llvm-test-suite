! GB DTP extension using:
! ftcx_dtp -qk -qnodeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn024a2.f
! opt variations: -qck -qnok -qdeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (poly-pointer as
!*                               function return)
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
    type base(k1,n1)    ! (4,15)
        integer, kind :: k1
        integer, len  :: n1
        character(n1) :: name = ''

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child    ! (4,15)
        integer(k1) :: id = 0

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase(b)
        class (base(4,*)), intent(in) :: b

        print *, b%name
    end subroutine

    subroutine printChild (b)
        class (child(4,*)), intent(in) :: b

        print *, b%name, b%id
    end subroutine
end module

program fpAssgn024a2
use m
    interface makeData
        function produceBasePtr (c)
        use m
            class (base(4,15)), pointer :: produceBasePtr
            character(*), intent(in) :: c
        end function

        function produceChildPtr (c, i)
        use m
            class (base(4,15)), pointer :: produceChildPtr
            character(*), intent(in) :: c
            integer*4, intent(in) :: i
        end function
    end interface

    type (base(4,15)), pointer :: b1
    class (base(4,15)), pointer :: b2

    b1 => makeData ('b1 test')

    b2 =>  makeData ('b1 test')

    call b1%print
    call b2%print

    deallocate (b1, b2)

    b1 => makeData ('test2', 10)

    b2 => makeData ('test2', 10)

    call b1%print
    call b2%print

    deallocate (b2)
end

function produceBasePtr (c)
use m
    class (base(4,15)), pointer :: produceBasePtr
    character(*), intent(in) :: c

    allocate (produceBasePtr)

    produceBasePtr%name = c
end function

function produceChildPtr (c, i)
use m
    class (base(4,15)), pointer :: produceChildPtr
    character(*), intent(in) :: c
    integer*4, intent(in) :: i

    type (child(4,15)), pointer :: tmp
    allocate (tmp)

    tmp%name = c
    tmp%id = i

    produceChildPtr => tmp
end function