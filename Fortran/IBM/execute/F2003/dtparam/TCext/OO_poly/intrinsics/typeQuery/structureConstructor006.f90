! GB DTP extension using:
! ftcx_dtp -ql -qreuse=none /tstdev/OO_poly/intrinsics/typeQuery/structureConstructor006.f
! opt variations: -qnol -qreuse=self

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureConstructor006.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/20/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                :
!*    MOLD: is specified using structure constructor with extensible
!*          non abstract type.
!*    A   : unlimited polymorphic, dynamic type is non-extensible.
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

    type SequenceBase(n2,k2,k3)    ! (20,4,4)
        integer, kind :: k2,k3
        integer, len  :: n2
        sequence
        integer(k2)      i
        integer(k3)      j
    end type
end module

program structureConstructor006
use m
    class(*), pointer :: arg1 => null()
    class(*), allocatable :: arg2

    allocate(integer::arg1)
    allocate(SequenceBase(20,4,4)::arg2)

    if(extends_type_of(arg1, Base(20,4)(1))) error stop 1_4
    if(extends_type_of(arg2, Base(20,4)(1))) error stop 2_4
    if(same_type_as(arg1, Base(20,4)(1))) error stop 3_4
    if(same_type_as(arg2, Base(20,4)(1))) error stop 4_4
end
