! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr021a.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr021a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (keyword and default
!*                               initializations with traditional structures)
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
    type p(k1)    ! (4)
        integer, kind        :: k1
        type(p(k1)), pointer :: z => null()
        integer(k1)          :: i = 1
    end type

    type q(k2,n1)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n1
        type(p(k2))   :: t
    end type


    type base(k3,n2)    ! (4,20)
        integer, kind  :: k3
        integer, len   :: n2
        type(q(k3,n2)) :: x
    end type

    type base1(k4,n3)    ! (4,20)
        integer, kind           :: k4
        integer, len            :: n3
        type(q(k4,n3)), pointer :: x => null()
    end type
end module

program fconstr021a
use m
    type (q(4,20)), target :: q1

    type (base(4,20)) :: b1
    type (base1(4,20)) :: bb1

    q1 = q(4,20) (t = p(4)())

    b1 = base(4,20) (x = q(4,20)(t = p(4)()))

    bb1 = base1(4,20) (x = q1)

    if (associated (q1%t%z) .or. (q1%t%i /= 1)) error stop 1_4

    if (associated (b1%x%t%z) .or. (b1%x%t%i /= 1)) error stop 2_4

    if (.not. associated (bb1%x, q1)) error stop 3_4
end
