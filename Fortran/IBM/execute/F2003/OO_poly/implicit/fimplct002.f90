!#######################################################################
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
    implicit type (base) (b), type (child) (c)

    type base
        integer*4 :: id = 0
    end type

    type, extends(base) :: child
        character*20 :: name = ''
    end type

    parameter (b1 = base(10), c1 = child(name = 'c1_constant'))

    if (b1%id /= 10) error stop 1_4

    if (b2%id /= 0) error stop 2_4

    if ((c1%id /= 0) .or. (c1%name /= 'c1_constant')) error stop 3_4

    if ((c%id /= 0) .or. (c%name /= '')) error stop 4_4
end
