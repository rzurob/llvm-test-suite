! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/intrinsics/typeQuery/structureConstructor010.f
! opt variations: -ql -qreuse=self

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureConstructor010.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/27/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                :
!*    A   : is specified using structure constructor with extensible
!*          non abstract type.
!*    MOLD: unlimited polymorphic, dynamic type is not extensible.
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type SequenceBase(k2,k3)    ! (4,4)
        integer, kind :: k2,k3
        sequence
        integer(k2)      i
        integer(k3)      j
    end type
end module

program structureConstructor010
use m
    class(*), pointer :: mold1 => null()
    class(*), allocatable :: mold2

    allocate(integer::mold1)
    allocate(SequenceBase(4,4)::mold2)

    if(extends_type_of(Base(4)(1), mold1)) error stop 1_4
    if(extends_type_of(Base(4)(1), mold2)) error stop 2_4
    if(same_type_as(Base(4)(1), mold1)) error stop 3_4
    if(same_type_as(Base(4)(1), mold2)) error stop 4_4
end
