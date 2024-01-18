! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr007.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr007.f
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
!*  DESCRIPTION                : structure constructor (empty base type)
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(base) :: child(k2,k3,n2)    ! (4,20,4,1,20)
        integer, kind             :: k2,k3
        integer, len              :: n2
        integer(k2)               :: id
        character(kind=k3,len=n2) :: name
    end type

    type(base(4,20)) :: b1_m = base(4,20)()
    type(child(4,20,4,1,20)) :: c1_m = child(4,20,4,1,20) (0, 'module data c1_m')
end module

program fconstr007
use m

    type (base(4,20)) :: b1 = base(4,20)()
    type (child(4,20,4,1,20)) :: c1 = child(4,20,4,1,20)(1, 'test data c1')

    print *, b1, b1_m

    if (c1_m%id /= 0) error stop 1_4
    if (c1_m%name /= 'module data c1_m') error stop 2_4

    if (c1%id /= 1) error stop 3_4
    if (c1%name /= 'test data c1') error stop 4_4
end
