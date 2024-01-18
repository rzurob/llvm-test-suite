! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr018d.f
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
! %POSTCMD: dcomp fconstr018d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (C484)
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

    type(base(4,4)) :: b1_m = base(4,4) (1, 10.0)
    type(child(4,4,1,20)) :: c1_m = child(4,4,1,20) (2, 2.0, 'c1_m')
end module

program fconstr018d
use m

    type (thirdGeneration(4,4,1,20,2)) :: t1 = thirdGeneration(4,4,1,20,2) (name = 't1', &
                    id = 1, base = base(4,4) (2), isSet = .true.)

    type (thirdGeneration(4,4,1,20,2)) :: t2, t3, t4

    t2 = thirdGeneration(4,4,1,20,2) (isSet = .true., base = b1_m, name = 't2', id = 2)
    t3 = thirdGeneration(4,4,1,20,2) (isSet = .true., child = c1_m, name = 't3')

    t4 = thirdGeneration(4,4,1,20,2) (child = child(4,4,1,20) (base = base(4,4)(3), name = 't4'), &
                          isSet = .true., name = 't4')

end
