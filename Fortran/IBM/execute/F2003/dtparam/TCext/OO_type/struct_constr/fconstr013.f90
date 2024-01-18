! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr013.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr013.f
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
!*  DESCRIPTION                : structure constructor (derived type with
!*                               private component)
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
        real(k2), private :: value = 0.0
    end type

    type (base(4,4)), save :: b1_m = base(4,4) (1, 10.0)

    contains

    logical function isBaseCorrect (b, intVal, realVal)
        type (base(4,4)), intent(in) :: b
        integer*4, intent(in) :: intVal
        real*4, intent(in) :: realVal

        isBaseCorrect = ( (b%id == intVal) .and. (b%value == realVal) )
    end function
end module

program fconstr013
use m

    type (base(4,4)) :: b1 = base(4,4) (1)
    type (base(4,4)) :: b2 = base(4,4) (id = 2)

    if (.not. isBaseCorrect (b1, 1, 0.0)) error stop 1_4
    if (.not. isBaseCorrect (b2, 2, 0.0)) error stop 2_4

    if (.not. isBaseCorrect (b1_m, 1, 10.0)) error stop 3_4
end
