! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr503.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr503.f
! %VERIFY: fconstr503.out:fconstr503.vf
! %STDIN:
! %STDOUT: fconstr503.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (array component with
!                               default initialization)
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

program fconstr503
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: i1(2) = (/1,2/)
    end type

    print *, base(4)()
    associate (x => base(4)())
        print *, x
        if (any(x%i1 /= (/1,2/))) error stop 1_4
    end associate
end
