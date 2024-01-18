! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg010a.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg010a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (VALUE attribute; pointer
!*                               component redefined in pointer assignment; not
!*                               affect the actual arg)
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
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    type (base(4)), target :: b1 = base(4) (10)
    type (child(4,1,20)), target :: c1 = child(4,1,20) (20, 'c1')

    type (child(4,1,20)), target :: c2 (2:4)
    type (base(4)), target :: b2 (3:7)
end module

module m1
use m
    type container(k3)    ! (4)
        integer, kind            :: k3
        class(base(k3)), pointer :: data => null()
    end type

    type container1(k4)    ! (4)
        integer, kind            :: k4
        class(base(k4)), pointer :: data(:) => null()
    end type
end module

program fArg010a
use m1
    type (container(4)) :: co
    type (container1(4)) :: co1

    call associateScalar (co, c1)

    if (associated (co%data)) error stop 1_4

    call associateArray1 (co1, b2)

    if (associated (co1%data)) error stop 2_4

    contains

    subroutine associateScalar (c, b)
        type (container(4)), value :: c
        class (base(4)), target, intent(in) :: b

        c%data => b
    end subroutine

    subroutine associateArray1 (c, b)
        type (container1(4)), value :: c
        class (base(4)), target, intent(in) :: b(:)

        c%data => b
    end subroutine
end
