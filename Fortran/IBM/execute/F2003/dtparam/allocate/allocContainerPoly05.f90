!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : January 13, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : The ALLOCATE Statement has an allocation-list
!*                               that is a Polymorphic Container Type
!*  SECONDARY FUNCTIONS TESTED : and the allocation-list contains Dummy Argument
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
!*  o  Dummy Arguments
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mod

    IMPLICIT NONE

    TYPE t(k,l1,l2)
        INTEGER, KIND :: k
        INTEGER(k), LEN :: l1
        INTEGER(k), LEN :: l2

        REAL(k) :: a( l1:l2,l2,(l2 - l1) )
    END TYPE t

END MODULE mod


MODULE cMod
    USE mod

    IMPLICIT NONE

    TYPE c(k,l1,l2)
        INTEGER, KIND :: k
        INTEGER, LEN :: l1
        INTEGER, LEN :: l2

        TYPE(t(k,l1,(l2 - l1))) :: content

        CONTAINS

            PROCEDURE, NOPASS :: NewC

    END TYPE c

    CONTAINS

        SUBROUTINE NewC(this, l1, l2, rc)
            CLASS(c(8,l1,l2)), ALLOCATABLE :: this
            INTEGER :: l1
            INTEGER :: l2

            INTEGER(4) :: rc

            INTEGER :: stat

            CHARACTER(255) :: errmsg


            ALLOCATE(this, STAT=stat, ERRMSG=errmsg)
            IF (stat /= 0) THEN
                PRINT *,"ALLOCATE(this(8,",l1,",",l2,"),STAT=",stat,') ',errmsg
                CALL zzrc( rc )
            END IF

        END SUBROUTINE NewC

END MODULE cMod


PROGRAM allocContainerPoly05
    USE cMod

    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: j
    INTEGER(4) :: rc = 0_4


    DO i = 1, 3
        DO j = 3, 1, -1
            rc = rc + 10_4
            CALL CheckIt(i, j, rc)
        END DO
    END DO


    CONTAINS

        SUBROUTINE CheckIt(n, m, rc)
            INTEGER :: n
            INTEGER :: m
            INTEGER(4) :: rc

            INTEGER :: checkA( 3 )

            CLASS(c(8,n,m)), ALLOCATABLE :: checkItem


            PRINT *, n, m, rc

            checkA( 1 ) = m - n - n + 1
            IF (checkA( 1 ) < 0) checkA( 1 ) = 0

            checkA( 2 ) = m - n
            IF (checkA( 2 ) < 0) checkA( 2 ) = 0

            checkA( 3 ) = checkA( 1 ) - n
            IF (checkA( 3 ) < 0) checkA( 3 ) = 0


            CALL checkItem%NewC(checkItem, n, m, rc)


            PRINT *, checkItem%k, checkItem%l1, checkItem%l2
            PRINT *, checkItem%content%k,&
                        checkItem%content%l1, checkItem%content%l2

            IF (checkItem%k /= 8)                   CALL zzrc( (rc + 1_4) )
            IF (checkItem%content%k /= 8)           CALL zzrc( (rc + 2_4) )

            IF (checkItem%l1 /= n)                  CALL zzrc( (rc + 3_4) )
            IF (checkItem%content%l1 /= n)          CALL zzrc( (rc + 4_4) )

            IF (checkItem%l2 /= m)                  CALL zzrc( (rc + 5_4) )
            IF (checkItem%content%l2 /= (m - n))    CALL zzrc( (rc + 6_4) )


            PRINT *, '(', SHAPE( checkItem%content%a ), ') (', checkA, ')'
            PRINT *

            IF ( ANY(SHAPE( checkItem%content%a ) /= checkA) )&
                                                    CALL zzrc( (rc + 7_4) )


            DEALLOCATE( checkItem )

        END SUBROUTINE CheckIt

END PROGRAM allocContainerPoly05
