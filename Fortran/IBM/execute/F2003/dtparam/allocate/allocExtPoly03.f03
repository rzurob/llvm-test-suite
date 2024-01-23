!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : January 13, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : The ALLOCATE Statement has an allocation-list
!*                               that is a Polymorphic of an Extended Derived
!*                               Type
!*  SECONDARY FUNCTIONS TESTED : and the allocation-list contains a Function
!*                               Return Value
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Where allocation-list (without type-spec:: and using:  Extended Derived
!*  Types) is:
!*  o  Polymorphic Derived Types
!*
!*  Types of allocation-list Objects:
!*  o  Function Return Value
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE baseMod

    IMPLICIT NONE

    TYPE base(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        CHARACTER(l1) :: typeOfStuff
        INTEGER(k1), POINTER :: stuff( : )
    END TYPE base

END MODULE baseMod


MODULE extMod
    USE baseMod

    IMPLICIT NONE

    TYPE, EXTENDS(base) :: ext(l2)
        INTEGER, LEN :: l2

        INTEGER(k1) :: moreStuff( l1,l2:l1 )
    END TYPE ext

END MODULE extMod


PROGRAM allocExtPoly03
    USE extMod

    IMPLICIT NONE

    INTERFACE
        FUNCTION NewExt( rc )
            USE extMod
            INTEGER(4) :: rc
            CLASS(ext(8,5,6)), POINTER :: NewExt
        END FUNCTION NewExt
    END INTERFACE


    CLASS(ext(8,5,6)), POINTER :: extPtr


    IF ( ASSOCIATED( extPtr ) )                         ERROR STOP 10_4


    extPtr => NewExt( 20_4 )


    IF (.NOT. ASSOCIATED( extPtr ))                     ERROR STOP 21_4

    IF (extPtr%k1 /= 8)                                 ERROR STOP 22_4
    IF (KIND( extPtr%stuff ) /= 8)                      ERROR STOP 23_4

    IF (extPtr%l1 /= 5)                                 ERROR STOP 24_4
    IF (extPtr%l2 /= 6)                                 ERROR STOP 25_4

    IF (LEN( extPtr%typeOfStuff ) /= 5)                 ERROR STOP 26_4
    IF ( ANY(SHAPE( extPtr%moreStuff ) /= [ 5,0 ]) )    ERROR STOP 27_4


    DEALLOCATE( extPtr )

    IF ( ASSOCIATED( extPtr ) )                         ERROR STOP 30_4

END PROGRAM allocExtPoly03


FUNCTION NewExt( rc )
    USE extMod

    INTEGER(4) :: rc

    CLASS(ext(8,5,6)), POINTER :: NewExt

    INTEGER :: stat
    CHARACTER(255) :: errmsg


    ALLOCATE(NewExt, STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        PRINT *, "ALLOCATE(NewExt,STAT=", stat, ") ", errmsg
        CALL zzrc( rc )
    END IF

END FUNCTION NewExt
