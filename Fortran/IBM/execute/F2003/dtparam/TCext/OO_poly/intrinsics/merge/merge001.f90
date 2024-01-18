! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/merge/merge001.f
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/21/2005
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    TSOURCE is scalar
!*    FSOURCE is scalar
!*    MASK is scalar
!*    Non-poly
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: i = 8
    end type
end module

program merge001
use m
    type(Base(4)) :: b1
    type(Base(4)) :: b2

    print *, merge(Base(4)(2), Base(4)(-2), .TRUE.)
    print *, merge(Base(4)(2), Base(4)(-2), .FALSE.)
    if(.NOT. same_type_as(merge(Base(4)(2), Base(4)(-2), .FALSE.), Base(4)(1))) &
     error stop 1_4

    b1%i = 3
    b2%i = 4

    print *, merge(b1, b2, .TRUE.)
    print *, merge(b1, b2, .FALSE.)
    if(.NOT. same_type_as(merge(b1, b2, .TRUE.), Base(4)(1))) &
     error stop 2_4
end
