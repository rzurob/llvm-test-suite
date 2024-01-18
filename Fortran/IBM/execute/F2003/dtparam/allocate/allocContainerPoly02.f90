!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : allocContainerPoly02
!*
!*  DATE                       : January  7, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : The ALLOCATE Statement has an allocation-list
!*                               that is a Polymorphic Container Type
!*  SECONDARY FUNCTIONS TESTED : and the allocation-list contains an Array
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Where allocation-list (without type-spec:: and using:  Container Derived
!*  Types) is:
!*  o  Polymorphic Derived Types
!*
!*  Types of allocation-list Objects:
!*  o  Arrays of Polymorphic Derived Types
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mod

    IMPLICIT NONE

    TYPE t(k,l1,l2)
        INTEGER, KIND :: k
        INTEGER(k), LEN :: l1
        INTEGER(k), LEN :: l2

        CHARACTER((l2 - l1)) :: a( (l1 + l2),(l1 * l2) )
    END TYPE t

END MODULE mod


MODULE cMod
    USE mod

    IMPLICIT NONE

    TYPE c(k,l1,l2)
        INTEGER, KIND :: k
        INTEGER, LEN :: l1
        INTEGER, LEN :: l2

        TYPE(t(k,l2,l1)) :: content( (l1 * l2) )
    END TYPE c

END MODULE cMod


PROGRAM allocContainerPoly02
    USE cMod

    IMPLICIT NONE

    INTERFACE
        SUBROUTINE CheckIt(n, m, l, rc)
            USE cMod

            IMPLICIT NONE

            INTEGER :: n
            INTEGER :: m
            INTEGER :: l
            INTEGER(4) :: rc
        END SUBROUTINE CheckIt
    END INTERFACE

    INTEGER :: i
    INTEGER :: j
    INTEGER :: k

    INTEGER(4) :: rc = 0_4


    DO i = 1, 3
        DO j = 1, 3
            DO k = 1, 3
                rc = rc + 200_4
                CALL CheckIt(i, j, k, rc)
            END DO
        END DO
    END DO

END PROGRAM allocContainerPoly02


SUBROUTINE CheckIt(n, m, l, rc)
    USE cMod

    IMPLICIT NONE

    INTEGER :: n
    INTEGER :: m
    INTEGER :: l
    INTEGER(4) :: rc

    INTEGER :: i
    INTEGER :: j
    INTEGER :: stat

    CHARACTER(255) :: errmsg

    CLASS(c(2,n,m)), POINTER :: pC( : )


    PRINT *, 'CheckIt(', n, ',', m, ',', l, ',', rc, ')'

    IF ( ASSOCIATED( pC ) )             CALL zzrc( rc )


    ALLOCATE(pC( l ), STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        PRINT *, "ALLOCATE(pC(", l, ",STAT=", stat, ") ", errmsg
        CALL zzrc( (rc + 1_4) )
    END IF


    IF (pC%k /= 2)                      CALL zzrc( (rc + 2_4) )
    IF (pC%l1 /= n)                     CALL zzrc( (rc + 3_4) )
    IF (pC%l2 /= m)                     CALL zzrc( (rc + 4_4) )

    IF ( ANY(SHAPE( pC ) /= [ l ]) )    CALL zzrc( (rc + 5_4) )


    DO i = 1, l
        IF (pC( i )%content%k /= 2)     CALL zzrc( (rc + 10_4 + INT(i, 4)) )
        IF (pC( i )%content%l1 /= m)    CALL zzrc( (rc + 20_4 + INT(i, 4)) )
        IF (pC( i )%content%l2 /= n)    CALL zzrc( (rc + 30_4 + INT(i, 4)) )

        IF ( ANY(SHAPE( pC( i )%content ) /= [ (m * n) ]) )&
                                        CALL zzrc( (rc + 40_4 + INT(i, 4)) )


        DO j = 1, (m * n)
            IF ( ANY(SHAPE( pC( i )%content( j )%a )&
                                /= [ (m + n), (m * n) ]) )&
                    CALL zzrc( (rc + 50_4 + (INT(i, 4) * 10_4) + INT(j, 4)) )

            IF (LEN( pC( i )%content( j )%a ) /= (n - m))&
                    CALL zzrc( (rc + 60_4 + (INT(i, 4) * 10_4) + INT(j, 4)) )
        END DO
    END DO


    DEALLOCATE( pC )

END SUBROUTINE CheckIt
