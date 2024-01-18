! GB DTP extension using:
! ftcx_dtp -qck -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/typeQuery/typeDeclaration002.f
! opt variations: -qnock -qnok -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeDeclaration002.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 10/25/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    MOLD: unlimited polymorphic and is a disassociated pointer or an
!*          unallocated allocatable.
!*    A   : polymorphic but not unlimited polymorphic. Declared type is
!*          extensible, and can be either abstract or non-abstract.
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
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      a
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i
    end type

    type, extends(Base) :: Child(k2,n1)    ! (4,1,10)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: c
    end type

    type, abstract :: ZeroAbstractParent(k3)    ! (4)
        integer, kind :: k3
    end type

    type, extends(ZeroAbstractParent) :: ZeroBase    ! (4)
    end type

    type, extends(Base) :: ZeroChild    ! (4)
    end type
end module

program typeDeclaration002
use m
    class(*), pointer :: mold1 => null()
    class(*), allocatable :: mold2
    class(Base(4)), pointer :: b1 => null()
    class(AbstractParent(4)), pointer :: ap1 => null()
    class(ZeroBase(4)), pointer :: z1 => null()
    class(ZeroAbstractParent(4)), pointer :: ap2 => null()

    !===== non zero-size derived type

    !-------- non abstract type

    if(.NOT. extends_type_of(b1, mold1)) error stop 1_4
    if(.NOT. extends_type_of(b1, mold2)) error stop 2_4
    if(same_type_as(b1, mold1)) error stop 3_4
    if(same_type_as(b1, mold2)) error stop 4_4

    allocate(Base(4)::mold1)
    if(.NOT. extends_type_of(b1, mold1)) error stop 5_4
    if(.NOT. same_type_as(b1, mold1)) error stop 6_4
    deallocate(mold1)
    allocate(Child(4,1,10)::mold1)
    if(extends_type_of(b1, mold1)) error stop 7_4
    if(same_type_as(b1, mold1)) error stop 8_4
    deallocate(mold1)

    !-------- abstract type

    if(.NOT. extends_type_of(ap1, mold1)) error stop 9_4
    if(.NOT. extends_type_of(ap1, mold2)) error stop 10_4
    if(same_type_as(ap1, mold1)) error stop 11_4
    if(same_type_as(ap1, mold2)) error stop 12_4

    allocate(Base(4)::mold2)
    if(extends_type_of(ap1, mold2)) error stop 13_4
    if(same_type_as(ap1, mold2)) error stop 14_4
    allocate(Base(4)::ap1)
    if(.NOT. extends_type_of(ap1, mold2)) error stop 15_4
    if(.NOT. same_type_as(ap1, mold2)) error stop 16_4
    deallocate(mold2)

    !===== zero-size derived type

    !-------- non abstract type

    if(.NOT. extends_type_of(z1, mold1)) error stop 17_4
    if(.NOT. extends_type_of(z1, mold2)) error stop 18_4
    if(same_type_as(z1, mold1)) error stop 19_4
    if(same_type_as(z1, mold2)) error stop 20_4

    allocate(ZeroBase(4)::mold1)
    if(.NOT. extends_type_of(z1, mold1)) error stop 21_4
    if(.NOT. same_type_as(z1, mold1)) error stop 22_4
    deallocate(mold1)
    allocate(ZeroChild(4)::mold1)
    if(extends_type_of(z1, mold1)) error stop 23_4
    if(same_type_as(z1, mold1)) error stop 24_4
    deallocate(mold1)

    !-------- abstract type

    if(.NOT. extends_type_of(ap2, mold1)) error stop 25_4
    if(.NOT. extends_type_of(ap2, mold2)) error stop 26_4
    if(same_type_as(ap2, mold1)) error stop 27_4
    if(same_type_as(ap2, mold2)) error stop 28_4

    allocate(ZeroBase(4)::mold2)
    if(extends_type_of(ap2, mold2)) error stop 29_4
    if(same_type_as(ap2, mold2)) error stop 30_4
    allocate(ZeroBase(4)::ap2)
    if(.NOT. extends_type_of(ap2, mold2)) error stop 31_4
    if(.NOT. same_type_as(ap2, mold2)) error stop 32_4
    deallocate(mold2)
end
