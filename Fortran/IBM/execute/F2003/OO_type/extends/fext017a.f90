!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext017a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 07, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : derived-type extension (base type empty
!*                               only with type-bound procedures)
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
!
! base type is interface
!
module m
    type base
        contains
        procedure, nopass :: type => baseType
    end type

    type (base) :: b1_m

    contains

    integer function baseType ()
        baseType = 1
    end function

end module

program fext017a
    use m

    type (base) :: b1

    if (b1%type() /= 1) error stop 1_4
    if (b1_m%type() /= 1) error stop 2_4

end
