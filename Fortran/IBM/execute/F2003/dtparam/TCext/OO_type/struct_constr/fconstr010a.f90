! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr010a.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr010a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 12, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (NOT all components have
!*                               default initialization, leading components
!*                               does NOT need keyword)
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
    type base(k1,k2)    ! (4,4)
        integer, kind :: k1,k2
        integer(k1)   :: id
        real(k2)      :: value = 0.0
    end type

    type, extends(base) :: child(k3,n1)    ! (4,4,1,20)
        integer, kind             :: k3
        integer, len              :: n1
        character(kind=k3,len=n1) :: name = ''
    end type

    type (base(4,4)), save :: b1_m = base(4,4) (10)
    type (child(4,4,1,20)), save :: c1_m = child(4,4,1,20) (20)
end module

program fconstr010a
use m

    type (base(4,4)) :: b1
    type (child(4,4,1,20)) :: c1

    b1 = base(4,4)(1)
    c1 = child(4,4,1,20)(2)

    if ((b1%id /= 1) .or. (b1%value /= 0.0)) error stop 1_4

    if ((c1%id /= 2) .or. (c1%value /= 0.0) .or. (c1%name /= '')) error stop 2_4

    c1 = child(4,4,1,20) (3, 1.0)

    if ((c1%id /= 3) .or. (c1%value /= 1.0) .or. (c1%name /= '')) error stop 3_4

    c1 = child(4,4,1,20) (4, name = 'data c1')

    if ((c1%id /= 4) .or. (c1%value /= 0.0) .or. &
        (c1%name /= 'data c1')) error stop 4_4

    if ((b1_m%id /= 10) .or. (b1_m%value /= 0.0)) error stop 5_4

    if ((c1_m%id /= 20) .or. (c1_m%value /= 0.0) .or. (c1_m%name /= '')) &
                    error stop 6_4
end
