!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : allocExtVariable04
!*  TEST CASE TITLE            : ALLOCATE() Statement with DTP
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : January 13, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : The ALLOCATE Statement has an allocation-list
!*                               that is an Extended Derived Type
!*  SECONDARY FUNCTIONS TESTED : and the allocation-list contains a FUNCTION
!*                               Return value that is an Array
!*
!*  DRIVER STANZA              : xlf2003
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
!*  o  Function Return values
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

        FUNCTION CreateLink(n, rc)
            INTEGER :: n
            INTEGER(4) :: rc

            TYPE(e(3,4,8)), POINTER :: CreateLink( : )


            INTEGER :: stat
            CHARACTER(255) :: errmsg

            ALLOCATE(CreateLink( n ), STAT=stat, ERRMSG=errmsg)
            IF (stat /= 0) THEN
                PRINT *, 'ALLOCATE(next(', n, ',STAT=', stat, ') ', errmsg
                CALL zzrc( rc )
            END IF

        END FUNCTION CreateLink

END MODULE eMod


PROGRAM allocExtVariable04
    USE eMod

    IMPLICIT NONE

    INTEGER, PARAMETER :: NODES = 3
    INTEGER :: stat

    TYPE(e(3,4,8)), POINTER :: listRoot( : )


    listRoot => listRoot%CreateLink(NODES, 10_4)
    CALL BuildNext(listRoot, NODES, 20_4)

    stat = VerifyItem(listRoot, NODES, 0_4)
    IF (stat /= 0)      CALL zzrc( stat )


    CONTAINS

        RECURSIVE SUBROUTINE BuildNext(item, n, rc)
            TYPE(e(3,4,8)), POINTER :: item( : )
            INTEGER :: n
            INTEGER(4) :: rc


            INTEGER :: i


            DO i = 1, n
                item( i )%next => item%CreateLink((n - 1), rc)
                CALL BuildNext(item( i )%next,&
                                (n - 1), (rc + 100_4 + INT(i, 4)))
            END DO

        END SUBROUTINE BuildNext


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

END PROGRAM allocExtVariable04
