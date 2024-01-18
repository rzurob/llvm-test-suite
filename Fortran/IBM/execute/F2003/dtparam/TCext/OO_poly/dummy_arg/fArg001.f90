! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg001.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg001.f
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

    elemental logical function baseEqual (b1, b2)
        class (base(4)), intent(in) :: b1, b2

        baseEqual = (b1%id == b2%id)
    end function
end module

program fArg001
use m
    type (base(4)) :: b1 = base(4) (10)

    type (child(4,1,20)) :: c1

    if (baseEqual (b2=base(4)(), b1=b1)) error stop 1_4

    if (.not. baseEqual (b1=child(4,1,20)(), b2=base(4)(1))) error stop 2_4

    if (.not. baseEqual (base(4)(10), b2 = child(4,1,20) (id=10))) error stop 3_4

    if (.not. baseEqual (c1, b2=base(4)())) error stop 4_4

    if (.not. baseEqual (b2 = c1, b1 = base(4) (1))) error stop 5_4
end
