!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : December 15, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : The ALLOCATE Statement has an allocation-list
!*                               that is an Extended Derived Type
!*  SECONDARY FUNCTIONS TESTED : and the allocation-list contains a Array
!*                               Variable
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
!*  o  Arrays of Basic Derived Type
!*
!*  Types of allocation-list Objects:
!*  o  Variable
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE bMod

    IMPLICIT NONE

    TYPE b(l1,l2,k1)
        INTEGER, LEN :: l1
        INTEGER, LEN :: l2
        INTEGER, KIND :: k1

        COMPLEX(k1) :: a( l1,l2 )
    END TYPE b

END MODULE bMod


MODULE eMod
    USE bMod

    IMPLICIT NONE

    TYPE, EXTENDS(b) :: e

        TYPE(e(l1,l2,k1)), POINTER :: next( : )

        CONTAINS

            PROCEDURE, NOPASS :: CreateLink

    END TYPE e

    CONTAINS

        INTEGER(4) FUNCTION CreateLink(this, n, rc)
            TYPE(e(3,4,8)), POINTER :: this( : )
            INTEGER :: n
            INTEGER(4) :: rc


            CHARACTER(255) :: errmsg

            ALLOCATE(this( n ), STAT=CreateLink, ERRMSG=errmsg)
            IF (CreateLink /= 0_4) THEN
                PRINT *, 'ALLOCATE(next(', n, ',STAT=', CreateLink, ') ', errmsg
                CreateLink = rc
            END IF

        END FUNCTION CreateLink

END MODULE eMod


PROGRAM allocExtVariable02
    USE eMod

    IMPLICIT NONE

    INTEGER, PARAMETER :: NODES = 3
    INTEGER :: stat

    TYPE(e(3,4,8)), POINTER :: listRoot( : )


    stat = listRoot%CreateLink(listRoot, NODES, 10_4)
    IF (stat /= 0)      CALL zzrc( stat )

    stat = BuildNext(listRoot, NODES, 20_4)
    IF (stat /= 0)      CALL zzrc( stat )

    stat = VerifyItem(listRoot, NODES, 0_4)
    IF (stat /= 0)      CALL zzrc( stat )


    CONTAINS

        RECURSIVE INTEGER(4) FUNCTION BuildNext(item, n, rc)
            TYPE(e(3,4,8)), POINTER :: item( : )
            INTEGER :: n
            INTEGER(4) :: rc


            INTEGER :: i


            i = 0
            BuildNext = 0

            DO WHILE ((i < n)  .AND.  (BuildNext == 0))
                i = i + 1

                BuildNext = item%CreateLink(item( i )%next, (n - 1), rc)
                IF ((BuildNext == 0)  .AND.  (n >= 0)) THEN
                    BuildNext =&
                        BuildNext(item( i )%next,&
                                  (n - 1), (rc + 100_4 + INT(i, 4)))
                END IF
            END DO

        END FUNCTION BuildNext


        RECURSIVE INTEGER(4) FUNCTION VerifyItem(item, n, rc)
            TYPE(e(3,4,8)), POINTER :: item( : )
            INTEGER :: n
            INTEGER(4) :: rc


            INTEGER :: i


            rc = INT((n * 10), 4) + (INT(n, 4) * rc)

            VerifyItem = 0
            IF (.NOT. ASSOCIATED( item )) THEN
                VerifyItem = 2000_4 + rc

            ELSE IF (SIZE( item ) /= n) THEN
                VerifyItem = 3000_4 + rc

            ELSE IF (item%k1 /= 8) THEN
                VerifyItem = 4000_4 + rc

            ELSE IF (item%l1 /= 3) THEN
                VerifyItem = 5000_4 + rc

            ELSE IF (item%l2 /= 4) THEN
                VerifyItem = 6000_4 + rc
            END IF


            i = 0
            DO WHILE ((i < n)  .AND.  (VerifyItem == 0))
                i = i + 1

                IF ( ANY(SHAPE( item( i )%a ) /= [ 3, 4 ]) ) THEN
                    VerifyItem = 7000_4 + rc + INT(i, 4)

                ELSE IF (n >= 0) THEN
                    VerifyItem =&
                        VerifyItem(item( i )%next, (n - 1), (rc + INT(i, 4)))
                END IF
            END DO

        END FUNCTION VerifyItem

END PROGRAM allocExtVariable02
