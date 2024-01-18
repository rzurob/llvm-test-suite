!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : dtpAttrSpecStmtData16
!*
!*  DATE                       : May  4, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  -- DATA statement
!*
!*  R532  data-stmt-constant  is  scalar-constant
!*                            or  scalar-constant-subobject
!*                            ...
!*
!*  R534  constant-subobject  is  designator
!*
!*  C566 (R534) constant-subobject shall be a subobject of a constant.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mConstants

    IMPLICIT NONE

    INTEGER, PARAMETER :: baseKind = 8
    INTEGER, PARAMETER :: baseSize = 5

END MODULE mConstants


MODULE mBase

    IMPLICIT NONE

    TYPE tBase(bKind,bLen)
        INTEGER, KIND :: bKind
        INTEGER, LEN :: bLen

        SEQUENCE

        INTEGER( bKind ) :: bSize
        INTEGER( bKind ) :: bVal( bLen )
    END TYPE tBase

END MODULE mBase


MODULE mData
    USE mBase
    USE mConstants

    INTEGER, PARAMETER :: mySize = 1

    INTEGER, PRIVATE :: i
    TYPE(tBase(baseKind,baseSize)), PARAMETER, PRIVATE :: bParam =&
            tBase(baseKind,baseSize)(baseSize,[ (i, i = 1, baseSize) ])

    TYPE(tBase(bParam%bKind,mySize)) :: baseData( bParam%bSize )
    DATA baseData / bParam%bSize * tBase(bParam%bKind,mySize)(mySize,[ -1 ]) /

END MODULE mData


PROGRAM dtpAttrSpecStmtData16
    USE mConstants
    USE mData

    IMPLICIT NONE

    INTEGER :: i


    IF (SIZE( baseData ) /= baseSize)           CALL zzrc( 10_4 )

    IF (baseData%bKind /= baseKind)             CALL zzrc( 11_4 )
    IF (baseData%bLen /= mySize)                CALL zzrc( 12_4 )

    DO i = 1, SIZE( baseData )
        IF (baseData( i )%bSize /= mySize)      CALL zzrc( (20_4 + INT(i, 4)) )
        IF (SIZE( baseData( i )%bVal ) /= mySize)&
                                                CALL zzrc( (30_4 + INT(i, 4)) )
        IF ( ANY(baseData( i )%bVal /= -1) )    CALL zzrc( (40_4 + INT(i, 4)) )
    END DO

END PROGRAM dtpAttrSpecStmtData16
