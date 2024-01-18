! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg001a.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg001a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (use of keyword for
!*                               argument; basic test)
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
        integer(k1)   :: id = 1
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'no-name'
    end type

    contains

    logical function b1LEb2 (b1, b2)
        class (base(4)), intent(in) :: b1, b2

        b1LEb2 = (b1%id <= b2%id)
    end function
end module


program fArg001a
use m
    type (base(4)) :: b1
    type (child(4,1,20)) :: c1 = child(4,1,20) (2, 'c1')

    if (b1LEb2(b2 = child(4,1,20)(1, 'test'), b1 = base(4)(3))) error stop 1_4

    b1%id = 3

    if (.not. b1LEb2 (b2 = b1, b1 = c1)) error stop 2_4
end
