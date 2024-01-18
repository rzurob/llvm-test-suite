! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr047.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr047.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (used for component with
!*                               default initialization)
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
    type A(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: i1 = 0
    end type
end module

module m1
use m
    type B(k2)    ! (4)
        integer, kind :: k2
        type(A(k2))   :: a1 = A(k2)(i1 = 1)
    end type
end module

program fconstr047
use m1
    type (B(4)) :: b1
    type (B(4)) :: b2 = B(4) (a1 = A(4)(i1 = 10))

    type (A(4)) :: a1

    type (A(4)) :: a2 = A(4) (i1 = 100)

    if (a1%i1 /= 0) error stop 1_4

    if (a2%i1 /= 100) error stop 2_4

    if (b1%a1%i1 /= 1) error stop 3_4

    if (b2%a1%i1 /= 10) error stop 4_4
end
