! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr032a1.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr032a1.f
! %VERIFY: fconstr032a1.out:fconstr032a1.vf
! %STDIN:
! %STDOUT: fconstr032a1.out
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
!*  DESCRIPTION                : structure constructor (array constructor used
!*                               for constructing poly-allocatable array
!*                               component)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printChild (c)
        class (child(4,1,*)), intent(in) :: c

        print *, c%id, c%name
    end subroutine

    subroutine printBase (c)
        class (base(4)), intent(in) :: c

        print *, c%id
    end subroutine
end module

module m1
use m
    type container(k3,n2)    ! (4,20)
        integer, kind                :: k3
        integer, len                 :: n2
        class(base(k3)), allocatable :: data (:)
    end type
end module

program fconstr032a
use m1
    type (container(4,20)) :: co1

    class (base(4)), allocatable :: c1, c2

    allocate (c1, source=child(4,1,20)(1,'test1'))
    allocate (c2, source=child(4,1,20)(2, 'test2'))

    co1 = container(4,20) ((/c1, c2/))

    if (size (co1%data) /= 2) error stop 1_4

    call co1%data(1)%print
    call co1%data(2)%print
end
