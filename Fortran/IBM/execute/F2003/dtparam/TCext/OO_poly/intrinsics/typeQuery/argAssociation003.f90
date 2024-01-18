! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/typeQuery/argAssociation003.f
! opt variations: -qnock -qnok -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation003.f
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
!*    arguments of another procedure. Dummy arguments are unlimited
!*    poly, non-pointer, and non-allocatable.
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

    type, extends(Base) :: Child(k2,n2)    ! (4,20,1,10)
        integer, kind             :: k2
        integer, len              :: n2
        character(kind=k2,len=n2) :: c
    end type
end module

program argAssociation003
use m
    type(Base(4,20)) :: b1
    type(Child(4,20,1,10)) :: c1
    class(AbstractParent(4,:)), allocatable :: ap1
    class(Base(4,:)), pointer :: b2 => null()
    class(Child(4,:,1,:)), allocatable :: c2
    class(*), pointer :: u1 => null()

    allocate(Child(4,20,1,10)::ap1)
    allocate(Child(4,20,1,10)::b2)
    allocate(Child(4,20,1,10)::c2)
    allocate(Base(4,20)::u1)
    call sub1(b1, b2, c1, c2, ap1, u1)

    contains

    subroutine sub1(arg1, arg2, arg3, arg4, arg5, arg6)
        class(*) :: arg1
        class(*) :: arg2
        class(*) :: arg3
        class(*) :: arg4
        class(*) :: arg5
        class(*) :: arg6

        if(extends_type_of(arg1, arg4)) error stop 1_4
        if(.NOT. extends_type_of(arg4, arg1)) error stop 2_4
        if(.NOT. extends_type_of(arg2, arg5)) error stop 3_4
        if(.NOT. extends_type_of(arg5, arg2)) error stop 4_4
        if(.NOT. extends_type_of(arg3, arg6)) error stop 5_4
        if(extends_type_of(arg6, arg3)) error stop 6_4

        if(same_type_as(arg1, arg2)) error stop 7_4
        if(.NOT. same_type_as(arg2, arg3)) error stop 8_4
        if(.NOT. same_type_as(arg3, arg4)) error stop 9_4
        if(.NOT. same_type_as(arg4, arg5)) error stop 10_4
        if(same_type_as(arg5, arg6)) error stop 11_4
    end subroutine
end
