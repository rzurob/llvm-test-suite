! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr500.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr500.f
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
!*  DESCRIPTION                : struture constructor (data-target shall be
!*                               allowable for pointer component as if in a data
!*                               pointer assignment)
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
    type seq1(k1,k2)    ! (4,4)
        integer, kind :: k1,k2
        sequence
        integer(k1)   :: i1
        integer(k2)   :: i2
    end type

    type base(k3)    ! (4)
        integer, kind              :: k3
        type(seq1(k3,k3)), pointer :: s1
    end type

    class (*), pointer :: x
end module

program fconstr500
use m
    type (seq1(4,4)), target :: s1
    type (base(4)) :: b1

    s1 = seq1(4,4) (1, 2)

    x => s1

    b1 = base(4) (s1 = x)

    if (.not. associated (b1%s1, s1)) error stop 1_4

    if ((b1%s1%i1 /=1) .or. (b1%s1%i2 /= 2)) error stop 2_4
end
