! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/implicit/fimplct002.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fimplct002.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : IMPLICIT statement (try named constants with
!*                               implicit statement)
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

program fimplct002
    implicit type (base(4)) (b), type (child(4,20)) (c)

    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = 0
    end type

    type, extends(base) :: child(n1)    ! (4,20)
        integer, len  :: n1
        character(n1) :: name = ''
    end type

    parameter (b1 = base(4)(10), c1 = child(4,20)(name = 'c1_constant'))

    if (b1%id /= 10) error stop 1_4

    if (b2%id /= 0) error stop 2_4

    if ((c1%id /= 0) .or. (c1%name /= 'c1_constant')) error stop 3_4

    if ((c%id /= 0) .or. (c%name /= '')) error stop 4_4
end
