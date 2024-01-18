! GB DTP extension using:
! ftcx_dtp -qck -ql /tstdev/OO_poly/intrinsics/typeQuery/typeDeclaration006.f
! opt variations: -qnock -qnol -qreuse=self

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeDeclaration006.f
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
!*  DATE                       : 10/26/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : 
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    A   : unlimited polymorphic and is a disassociated pointer or an
!*          unallocated allocatable.
!*    MOLD: unlimited polymorphic and is neither a disassociated pointer
!*          or an unallocated allocatable. Dynamic type can be either
!*          extensible or non-extensible, including intrinsics and
!*          sequence types, but cannot be abstract.
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(k2,n2)    ! (20,4,1,10)
        integer, kind             :: k2
        integer, len              :: n2
        character(kind=k2,len=n2) :: c
    end type

    type SequenceBase(n3,k3,k4)    ! (20,4,4)
        integer, kind :: k3,k4
        integer, len  :: n3
        sequence
        integer(k3)      i
        integer(k4)      j
    end type
end module

program typeDeclaration006
use m
    class(*), pointer :: arg1 => null()
    class(*), allocatable :: arg2
    class(*), pointer :: u1 => null()

    !--------- both MOLD and A are unlimited polymorphic
    !--------- and either disassociated pointer or unallocated
    !--------- allocatable
    if(.NOT. extends_type_of(arg1, u1)) error stop 1_4
    if(.NOT. extends_type_of(arg2, u1)) error stop 2_4
    if(same_type_as(arg1, u1)) error stop 3_4
    if(same_type_as(arg2, u1)) error stop 4_4

    !--------- dynamic type of MOLD is intrinsic

    allocate(integer::u1)
    if(extends_type_of(arg1, u1)) error stop 5_4
    if(same_type_as(arg1, u1)) error stop 6_4
    deallocate(u1)
    if(.NOT. extends_type_of(arg1, u1)) error stop 7_4
    if(same_type_as(arg1, u1)) error stop 8_4

    !--------- dynamic type of A is sequence type

    allocate(SequenceBase(20,4,4)::u1)
    if(extends_type_of(arg2, u1)) error stop 9_4
    if(same_type_as(arg2, u1)) error stop 10_4
    deallocate(u1)
    if(.NOT. extends_type_of(arg2, u1)) error stop 11_4
    if(same_type_as(arg2, u1)) error stop 12_4

    !--------- dynamic type of A is extensible

    allocate(Base(20,4)::u1)
    allocate(Child(20,4,1,10)::arg1)
    if(.NOT. extends_type_of(arg1, u1)) error stop 13_4
    if(same_type_as(arg1, u1)) error stop 14_4
    deallocate(u1, arg1)
    if(.NOT. extends_type_of(arg1, u1)) error stop 15_4
    if(same_type_as(arg1, u1)) error stop 16_4
end
