! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr017d1.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp fconstr017d1.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (C483, C485, C487)
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
        integer, kind     :: k1,k2
        integer(k1)       :: id
        real(k2), private :: value = 1.0
    end type

    type, extends(base) :: child(k3,n1)    ! (4,4,1,20)
        integer, kind             :: k3
        integer, len              :: n1
        character(kind=k3,len=n1) :: name
    end type

    type, extends(child) :: thirdGeneration(k4)    ! (4,4,1,20,2)
        integer, kind :: k4
        logical(k4)   :: isSet
    end type

end module

program fconstr017d1
use m

    type (child(4,4,1,20)) :: c1 = child(4,4,1,20) (name = 'c1', value = 1.0, id = 10)
    type (child(4,4,1,20)) :: c2 = child(4,4,1,20) (name = 'c1', id = 10, id = 20)

    type (thirdGeneration(4,4,1,20,2)) :: t1 = thirdGeneration(4,4,1,20,2) ( &
                    value = 0.0, isSet=.true., name = 't1', id = 1)

    type (thirdGeneration(4,4,1,20,2)) :: t2, t3

    t2 = thirdGeneration(4,4,1,20,2) (name = 't2', id = 2, isSet=.true., name = 't2')
    t3 = thirdGeneration(4,4,1,20,2) (name = 't3', id =3)

end
