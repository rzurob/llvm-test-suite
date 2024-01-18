!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : allocContainerVariable05
!*
!*  DATE                       : January 13, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : The ALLOCATE Statement has an allocation-list
!*                               that is a Derived Type Container
!*  SECONDARY FUNCTIONS TESTED : and the allocation-list contains a Dummy
!*                               Argument
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
!*  o  Dummy Arguments
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

END MODULE baseMod


PROGRAM allocContainerVariable05
    USE baseMod

    IMPLICIT NONE

    INTEGER(4) :: stat

    TYPE(node(2,5)), POINTER :: root => NULL( )


    stat = MakeNodes(root, 5, 10_4)
    IF (stat /= 0)  CALL zzrc( stat )

    CALL CheckNodes(root, 5, 100_4)


    CONTAINS


        RECURSIVE INTEGER(4) FUNCTION MakeNodes(root, n, rc)
            TYPE(node(2,*)), POINTER :: root
            INTEGER :: n
            INTEGER(4) :: rc


            CHARACTER(255) :: errmsg


            MakeNodes = 0


            IF ((MakeNodes == 0)    .AND.&
                (.NOT. ASSOCIATED( root ))) THEN
                ALLOCATE(root, STAT=MakeNodes, ERRMSG=errmsg)

                IF (MakeNodes /= 0) THEN
                    PRINT *, "MakeNodes():  ALLOCATE(root,STAT=",&
                                            MakeNodes, ") ", errmsg
                    MakeNodes = rc
                END IF

                IF (root%l /= n)  then
                    print *, 'root%l is expected as ', n, '; actual is', root%l
                    MakeNodes = rc + 1_4
                end if
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
            TYPE(node(2,*)), POINTER :: root
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

END PROGRAM allocContainerVariable05
