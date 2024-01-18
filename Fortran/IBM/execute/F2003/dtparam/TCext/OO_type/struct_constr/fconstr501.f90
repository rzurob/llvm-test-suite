! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr501.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr501.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (self data in structure
!                               constructor)
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
        integer(k1)   :: id = -1
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'
    end type
end module

program fconstr501
use m
    type (child(4,1,20)) :: c1, c2 (3)

    c1 = child(4,1,20) (1, 'c1')

    c2 = (/child(4,1,20)(10, 'c1_1'), child(4,1,20)(20, 'c1_2'), child(4,1,20)(30, 'c1_3')/)

    c1 = child(4,1,20) (base = c1%base, name = 'c1')

    if ((c1%id /= 1) .or. (c1%name /= 'c1')) error stop 1_4

    c2 (1:2) = (/(child(4,1,20) (c2(i)%id, c2(i)%name), i=2,3)/)

    if (any (c2%id /= (/20, 30, 30/))) error stop 2_4

    if (any (c2%name /= (/'c1_2', 'c1_3', 'c1_3'/))) error stop 3_4
end
