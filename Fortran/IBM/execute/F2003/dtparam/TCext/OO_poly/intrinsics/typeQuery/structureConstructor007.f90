! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/intrinsics/typeQuery/structureConstructor007.f
! opt variations: -qck -qnol -qnodeferredlp

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureConstructor007.f
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
!*  DATE                       : 10/27/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    A   : is specified using structure constructor with extensible
!*          non abstract type.
!*    MOLD: non polymorphic
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

    type, extends(Base) :: Child(n2)    ! (20,4,10)
        integer, len  :: n2
        character(n2) :: c
    end type
end module

program structureConstructor007
use m
    type(Base(20,4)) :: mold1
    type(Child(20,4,10)), target :: mold2
    type(Base(:,4)), pointer :: mold3 => null()
    type(Child(:,4,:)), allocatable :: mold4
    type(Base(20,4)), parameter :: mold5 = Base(20,4)(1)

    if(.NOT. extends_type_of(Base(20,4)(1), mold1)) error stop 1_4
    if(extends_type_of(Base(20,4)(1), mold2)) error stop 2_4
    if(.NOT. extends_type_of(Child(20,4,10)(1, "abc"), mold3)) error stop 3_4
    if(extends_type_of(Base(20,4)(1), mold4)) error stop 4_4
    if(.NOT. extends_type_of(Base(20,4)(1), mold5)) error stop 5_4

    if(.NOT. same_type_as(Base(20,4)(1), mold1)) error stop 6_4
    if(same_type_as(Base(20,4)(1), mold2)) error stop 7_4
    if(same_type_as(Child(20,4,10)(1, "abc"), mold3)) error stop 8_4
    if(.NOT. same_type_as(Child(20,4,10)(1, "abc"), mold4)) error stop 9_4
    if(.NOT. same_type_as(Base(20,4)(1), mold5)) error stop 10_4
end
