! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr014.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr014.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 14, 2003
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
        real(k2), private :: value = 10.0
    end type

    type, extends (base) :: child(k3,n1)    ! (4,4,1,20)
        integer, kind             :: k3
        integer, len              :: n1
        character(kind=k3,len=n1) :: name
    end type

    type (child(4,4,1,20)), save :: c1_m = child(4,4,1,20) (100, 2.0, 'c1_m')

    contains

    logical function isBaseCorrect (b, intVal, realVal)
        type (base(4,4)), intent(in) :: b
        integer*4, intent(in) :: intVal
        real*4, intent(in) :: realVal

        isBaseCorrect = ( (b%id == intVal) .and. (b%value == realVal) )
    end function
end module

program fconstr014
use m

    type (child(4,4,1,20)) :: c1 = child(4,4,1,20) (id = 1, name = 'c1')
    type (child(4,4,1,20)) :: c2 = child(4,4,1,20) (2, name = 'c2')

    if (.not. isBaseCorrect (c1%base, 1, 10.0)) error stop 1_4
    if (c1%name /= 'c1') error stop 2_4

    if (.not. isBaseCorrect (c1_m%base, 100, 2.0)) error stop 3_4
    if (c1_m%name /= 'c1_m') error stop 4_4

    if (.not. isBaseCorrect (c2%base, 2, 10.0)) error stop 5_4
    if (c2%name /= 'c2') error stop 6_4
end
