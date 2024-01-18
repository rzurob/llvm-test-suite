! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr028a.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr028a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/23/2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor for derived type with
!*                               type-bound proc
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure, nopass :: print => basePrint
    end type

    type, extends(base) :: child(k2,n2)    ! (4,20,1,20)
        integer, kind             :: k2
        integer, len              :: n2
        character(kind=k2,len=n2) :: name = ''
    end type

    type, extends(child) :: thirdGeneration(k3)    ! (4,20,1,20,1)
        integer, kind :: k3
        logical(k3)   :: isSet
    end type

    contains

    subroutine basePrint
        print *, 'base'
    end subroutine
end module

program fconstr028a
use m

    type (child(4,20,1,20)) :: c1 = child(4,20,1,20) ('c1')
    type (child(4,20,1,20)) :: c2 = child(4,20,1,20) (name = 'c2')

    type (thirdGeneration(4,20,1,20,1)) :: t1 = thirdGeneration(4,20,1,20,1)(isSet = .true.)
    type (thirdGeneration(4,20,1,20,1)) :: t2 = thirdGeneration(4,20,1,20,1) (name = 't2', isSet=.true.)
    type (thirdGeneration(4,20,1,20,1)) :: t3 = thirdGeneration(4,20,1,20,1) ('t3', .true.)

    call t3%print()


    ! validate all variables

    if (c1%name /= 'c1') error stop 1_4

    if (c2%name /= 'c2') error stop 2_4

    if ((t1%name /= '') .or. (t1%name /= t1%child%name)) error stop 3_4

    if (.not. t1%isSet) error stop 4_4

    if ((t2%name /= 't2') .or. (t2%name /= t2%child%name)) error stop 5_4

    if (.not. t2%isSet) error stop 6_4

    if ((t3%name /= 't3') .or. (t3%name /= t3%child%name)) error stop 7_4

    if (.not. t3%isSet) error stop 8_4

end
