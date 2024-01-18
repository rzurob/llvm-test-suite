! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc005a2.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc005a2.f
! %VERIFY: falloc005a2.out:falloc005a2.vf
! %STDIN:
! %STDOUT: falloc005a2.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (defined binary operator used in
!                               source-expr in ALLOCATE stmt)
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

    interface operator (+)
        function addB1B2 (b1, b2)
        import base
            type (base(4)), intent(in) :: b1, b2
            type (base(4)) addB1B2
        end function

        function addC1C2 (c1, c2)
        import child
            type (child(4,1,*)), intent(in) :: c1, c2
            type (child(4,1,20)) addC1C2
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

type (base(4)) function addB1B2 (b1, b2)
use m, only: base
    type (base(4)), intent(in) :: b1, b2

    addB1B2%id = b1%id + b2%id
end function

type (child(4,1,20)) function addC1C2 (c1, c2)
use m, only: child
    type (child(4,1,*)), intent(in) :: c1, c2

    addC1C2%id = c1%id + c2%id
    addC1C2%name = trim (c1%name)//' '//trim(c2%name)
end function

program falloc005a2
use m
    class (base(4)), pointer :: b1
    class (base(4)), allocatable :: b2, b3(:)

    allocate (b1, source=child(4,1,20)(1,'xlftest')+child(4,1,20)(100, '101'))

    allocate (b2, source=base(4)(10)+base(4)(1))

    allocate (b3(10:11), source=b2 + base(4)(100))

    call b1%print

    call b2%print

    call b3(10)%print
    call b3(11)%print

    deallocate (b1, b2, b3)
end
