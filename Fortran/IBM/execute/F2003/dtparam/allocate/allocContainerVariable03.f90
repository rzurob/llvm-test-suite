!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : allocContainerVariable03
!*
!*  DATE                       : January 13, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : The ALLOCATE Statement has an allocation-list
!*                               that is a Derived Type Container
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
!*  Where allocation-list (without type-spec:: and using:  a Container
!*  Derived Type) is:
!*  o  A variable of Basic Derived Type
!*
!*  Types of allocation-list Objects:
!*  o  Function Return Value
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE baseMod

    IMPLICIT NONE

    TYPE node(k,l)
        INTEGER, KIND :: k
        INTEGER, LEN :: l

        INTEGER(k) :: items( l )
        TYPE(node(k,(l - 1))), POINTER :: left => NULL( )
        TYPE(node(k,(l - 1))), POINTER :: right => NULL( )
    END TYPE

    CONTAINS

        FUNCTION NewNode(n, rc)
            INTEGER :: n
            INTEGER(4) :: rc

            TYPE(node(2,n)), POINTER :: NewNode


            INTEGER :: stat

            CHARACTER(255) :: errmsg


            ALLOCATE(NewNode, STAT=stat, ERRMSG=errmsg)
            IF (stat /= 0) THEN
                PRINT *, "NewNode():  ALLOCATE(NewNode,STAT=",stat, ") ",errmsg
                CALL zzrc( rc )
            END IF

        END FUNCTION NewNode

END MODULE baseMod


PROGRAM allocContainerVariable03
    USE baseMod

    IMPLICIT NONE

    INTEGER(4) :: stat

    TYPE(node(2,5)), POINTER :: root => NULL( )


    stat = MakeNodes(root, 5, 10_4)
    IF (stat /= 0)  CALL zzrc( stat )

    CALL CheckNodes(root, 5, 100_4)


    CONTAINS


        RECURSIVE INTEGER(4) FUNCTION MakeNodes(root, n, rc)
            TYPE(node(2,n)), POINTER :: root
            INTEGER :: n
            INTEGER(4) :: rc


            MakeNodes = 0
!            PRINT *, "MakeNodes(", root%l, ',', n, ',', rc, ')'

!            IF (root%l /= n)    MakeNodes = rc + 1_4

            IF ((MakeNodes == 0)    .AND.&
                (.NOT. ASSOCIATED( root ))) THEN
                root => NewNode(n, rc)
            END IF

            IF ((root%l > 0)    .AND.&
                (MakeNodes == 0)) THEN
                MakeNodes = MakeNodes(root%left, (n - 1), (rc + 10_4))

                IF ((MakeNodes == 0)    .AND.&
                    (MOD(root%l, 2) > 0)) THEN
                    MakeNodes = MakeNodes(root%right, (n - 1), (rc + 15_4))
                END IF
            END IF

        END FUNCTION MakeNodes


        RECURSIVE SUBROUTINE CheckNodes(root, n, rc)
            TYPE(node(2,n)), POINTER :: root
            INTEGER :: n
            INTEGER(4) :: rc


            PRINT *, "CheckNodes(", root%l, ',', n, ',', rc, ')'
            IF (.NOT. ASSOCIATED( root ))   CALL zzrc( rc )

            IF (root%l /= n)                CALL zzrc( (rc + 1_4) )
            IF (SIZE( root%items ) /= n)    CALL zzrc( (rc + 2_4) )

            IF (n > 0) THEN
                CALL CheckNodes(root%left, (n - 1), (rc + 10_4))

                IF (MOD(n, 2) > 0) THEN
                    CALL CheckNodes(root%right, (n - 1), (rc + 15_4))
                END IF
            END IF

        END SUBROUTINE CheckNodes

END PROGRAM allocContainerVariable03
