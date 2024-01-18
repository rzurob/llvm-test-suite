! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/asynchIO/attribute/otherAttr/asynchPointerAttr01.f
! opt variations: -ql -qreuse=self -qreuse=none

!*  ===================================================================
!*
!*                               Interactions with Other Attributes
!*
!*  DATE                       : February  9, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : Interactions with the POINTER Attribute
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  5.1 Type declaration statements
!*
!*  R501 type-declaration-stmt  is  declaration-type-spec [ [ , attr-spec ]&
!*                                      &... :: ] entity-decl-list
!*
!*  R502 declaration-type-spec  is  intrinsic-type-spec
!*
!*  R503 attr-spec  is  access-spec
!*                  or  ALLOCATABLE
!*                  or  ASYNCHRONOUS
!*  ...
!*                  or  POINTER
!*
!*  5.2.10 POINTER statement
!*
!*  R540 pointer-stmt  is  POINTER [ :: ] pointer-decl-list
!*  R541 pointer-decl  is  object-name [ ( deferred-shape-spec-list ) ]
!*                     or  proc-entity-name
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mPoint
    TYPE tPoint(K1,K2)    ! (4,4)
        INTEGER, KIND :: K1,K2
        REAL(K1)      :: x
        REAL(K2)      :: y
    END TYPE tPoint
END MODULE mPoint


MODULE mPixel
    USE mPoint

    TYPE, EXTENDS( tPoint ) :: tPixel    ! (4,4)
        INTEGER(K1) :: red
        INTEGER(K1) :: green
        INTEGER(K1) :: blue
    END TYPE tPixel
END MODULE mPixel


PROGRAM asynchPointerAttr01
    USE mPoint
    USE mPixel

    IMPLICIT NONE

    TYPE(tPoint(4,4)), TARGET :: p1
    TYPE(tPixel(4,4)), TARGET :: p2

    CLASS(tPoint(4,4)), POINTER :: ptX
    CLASS(tPoint(4,4)), POINTER, ASYNCHRONOUS :: ptY


    p1 = tPoint(4,4)(12.0, 65.0)
    p2 = tPixel(4,4)(65.0,12.0, 234,123,65)


    ptX => p1
    ptY => p2

    WRITE(6, 100) ptX%x, ptX%y, ptY%x, ptY%y


    ptX => p2
    ptY => p1

    WRITE(6, 100) ptX%x, ptX%y, ptY%x, ptY%y


100 FORMAT('ptX = (',F4.1,',',F4.1,'), ptY = (',F4.1,',',F4.1,')')

END PROGRAM asynchPointerAttr01
