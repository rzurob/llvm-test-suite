! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg028a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (a test on sequence
!                               association)
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

    type, extends(base) :: child    ! (4)
        class (base(k1)), pointer :: data => null()

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    recursive subroutine printChild (b)
        class (child(4)), intent(in) :: b

        if (associated (b%data)) then
            print *, 'id =', b%id, ', and data is:'
            call b%data%print
        else
            print *, 'id =', b%id, ', data is null'
        end if
    end subroutine

    subroutine printVal (b)
        class (base(4)), intent(in) :: b(3)

        call b(1)%print
        call b(2)%print
        call b(3)%print
    end subroutine
end module

program fArg028a
use m
    type (child(4)), target :: c1 (3)

    type (base(4)), target :: b1 (2)
    class (base(4)), pointer :: c2, c3(:)


    b1 = (/base(4)(10), base(4)(20)/)

    allocate (c2, source=child(4)(30, b1(1)))


    c1 = (/child(4)(1, b1(2)), child(4)(2), child(4)(3,c2)/)

    allocate (c3(3), source=c1)


    call printVal ((/c2, c3(2:3)/))

    deallocate (c2, c3)
end
