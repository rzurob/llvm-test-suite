!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : dtpAttrSpecStmtData17
!*
!*  DATE                       : May 11, 2009
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
!*                            or  signed-int-literal-constant
!*                            or  signed-real-literal-constant
!*                            or  null-init
!*                            or  structure-constructor
!*
!*  C563  (R532) If a DATA statement constant value is a named constant or
!*        a structure constructor, the named constant or derived type shall
!*        have been declared previously in the scoping unit or made accessible
!*        by use or host association.
!*
!*  (scalar-constant by Host Association)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mBase

    IMPLICIT NONE

    TYPE tBase(bLen,bKind)
        INTEGER, LEN :: bLen
        INTEGER, KIND :: bKind

        REAL(bKind) :: array( bLen )
        TYPE(tBase(bLen,bKind)), POINTER :: basePtr
    END TYPE tBase

END MODULE mBase


PROGRAM dtpAttrSpecStmtData17
    USE mBase

    IMPLICIT NONE


    INTEGER :: i
    TYPE(tBase(5,8)), PARAMETER :: base =&
        tBase(5,8)([ (REAL(i, 8), i = 1, 5) ],NULL( ))


    CALL BaseInit( )


    CONTAINS

        SUBROUTINE BaseInit( )

            TYPE(tBase(5,8)) :: localBase( 10 )
            DATA localBase / 10 * base /


            DO i = 1, 10
                IF (localBase%bLen /= 5)    CALL zzrc( (10_4 + INT(i, 4)) )
                IF (localBase%bKind /= 8)   CALL zzrc( (20_4 + INT(i, 4)) )

                IF ( ANY(localBase( i )%array /= base%array) )  &
                                            CALL zzrc( (30_4 + INT(i, 4)) )

                IF ( ASSOCIATED( localBase( i )%basePtr ) )&
                                            CALL zzrc( (40_4 + INT(i, 4)) )

                IF (localBase( i )%bLen /= 5)&
                                            CALL zzrc( (50_4 + INT(i, 4)) )
                IF (localBase( i )%bKind /= 8)&
                                            CALL zzrc( (60_4 + INT(i, 4)) )
            END DO

        END SUBROUTINE BaseInit

END PROGRAM dtpAttrSpecStmtData17
