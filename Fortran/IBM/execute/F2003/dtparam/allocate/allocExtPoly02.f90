!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : allocExtPoly02
!*
!*  DATE                       : January  9, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : The ALLOCATE Statement has an allocation-list
!*                               that is a Polymorphic of an Extended Derived
!*                               Type
!*  SECONDARY FUNCTIONS TESTED : and the allocation-list contains an Array
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
!*  o  Arrays of Polymorphic Derived Types
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE baseMod

    IMPLICIT NONE

    TYPE base(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        INTEGER(k1) :: stuff( l1 )
    END TYPE base

END MODULE baseMod


MODULE extMod
    USE baseMod

    IMPLICIT NONE

    TYPE, EXTENDS(base) :: ext(l2)
        INTEGER, LEN :: l2

        INTEGER(k1) :: moreStuff( l1,l2 )
    END TYPE ext

END MODULE extMod


MODULE varMod
    USE extMod

    IMPLICIT NONE

    INTEGER :: length
    INTEGER(4) :: rc

    CLASS(ext(4,7,3)), POINTER :: cExt( : )

END MODULE varMod


MODULE allocMod
    USE varMod

    IMPLICIT NONE

    CONTAINS

        INTEGER(4) FUNCTION AllocFunc( )

            CHARACTER(255) :: errmsg

            ALLOCATE(cExt( length ), STAT=AllocFunc, ERRMSG=errmsg)
            IF (AllocFunc /= 0_4) THEN
                PRINT *, "ALLOCATE(cExt(",length,",STAT=",AllocFunc,") ",errmsg
                AllocFunc = rc
            END IF

        END FUNCTION AllocFunc

END MODULE allocMod


PROGRAM allocExtPoly02
    USE allocMod

    IMPLICIT NONE

    INTERFACE
        INTEGER(4) FUNCTION Check( )
            USE allocMod
            IMPLICIT NONE
        END FUNCTION Check
    END INTERFACE

    INTEGER(4) :: stat


    DO length = 1, 10
        rc = INT((length * 20), 4)

        stat = AllocFunc( )
        IF (stat == 0)  stat = Check( )

        IF (stat /= 0)  CALL zzrc( stat )
    END DO

END PROGRAM allocExtPoly02


INTEGER(4) FUNCTION Check( )
    USE varMod

    IMPLICIT NONE

    INTEGER :: i


    Check = 0_4

    IF (SIZE( cExt ) /= length)     Check = rc + 1_4

    IF (cExt%k1 /= 4)               Check = rc + 2_4
    IF (cExt%l1 /= 7)               Check = rc + 3_4
    IF (cExt%l2 /= 3)               Check = rc + 4_4

    DO i = 1, length
        IF (KIND( cExt( i )%stuff ) /= 4)                   Check = rc + 5_4
        IF (KIND( cExt( i )%moreStuff ) /= 4)               Check = rc + 6_4

        IF ( ANY(SHAPE( cExt( i )%stuff ) /= [ 7 ]) )       Check = rc + 7_4
        IF ( ANY(SHAPE( cExt( i )%moreStuff ) /= [ 7, 3 ]) )Check = rc + 8_4

        IF (cExt( i )%base%k1 /= 4)                         Check = rc + 9_4
        IF (KIND( cExt( i )%base%stuff ) /= 4)              Check = rc + 10_4

        IF (cExt( i )%base%l1 /= 7)                         Check = rc + 11_4
        IF ( ANY(SHAPE( cExt( i )%base%stuff ) /= [ 7 ]) )  Check = rc + 12_4
    END DO

END FUNCTION Check
