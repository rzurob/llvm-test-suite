! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/typeQuery/argAssociation001.f
! opt variations: -qck -qnok -qnol -qdeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/25/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : Arguments of the intrinsics are dummy
!*    arguments of another procedure. Dummy arguments are non-poly,
!*    non-pointer, and non-allocatable.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child(n2)    ! (4,20,10)
        integer, len  :: n2
        character(n2) :: c
    end type
end module

program argAssociation001
use m
    type(Base(4,20)) :: b1
    class(Base(4,20)), pointer :: b2 => null()
    type(Child(4,20,10)) :: c1
    class(Child(4,20,10)), allocatable :: c2

    allocate(Child(4,20,10)::b2)
    allocate(c2)
    call sub1(b1, b2, c1, c2)

    contains

    subroutine sub1(arg1, arg2, arg3, arg4)
        type(Base(4,*)) :: arg1
        type(Base(4,*)) :: arg2
        type(Child(4,*,*)) :: arg3
        type(Child(4,*,*)) :: arg4

        if(.NOT. extends_type_of(arg1, arg2)) error stop 1_4
        if(.NOT. extends_type_of(arg2, arg1)) error stop 2_4
        if(.NOT. extends_type_of(arg3, arg1)) error stop 3_4
        if(.NOT. extends_type_of(arg4, arg1)) error stop 4_4

        if(.NOT. same_type_as(arg1, arg2)) error stop 5_4
        if(same_type_as(arg2, arg3)) error stop 6_4
        if(.NOT. same_type_as(arg3, arg4)) error stop 7_4
    end subroutine
end
