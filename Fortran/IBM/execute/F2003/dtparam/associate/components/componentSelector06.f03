!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : September 15, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is the Component of a variable
!*                               of Dervied Type
!*  SECONDARY FUNCTIONS TESTED : The Component is a Polymorphic POINTER
!*                               (or ALLOCATABLE) of another Derived Type
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Testing where the selector is a Component of a Derived Type that uses
!*  Type Parameters, and the Component:
!*  * Is a Polymorphic Derived Type (ALLOCATABLE, POINTER)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE tBaseMod
    IMPLICIT NONE

    TYPE tBase(l,k)
        INTEGER, LEN :: l
        INTEGER, KIND :: k

        CHARACTER(2) :: label
        LOGICAL(k) :: visitList( l )
        CLASS(*), POINTER :: basePtr
    END TYPE tBase

END MODULE tBaseMod


MODULE tExtendedMod
    USE tBaseMod

    IMPLICIT NONE

    TYPE, EXTENDS(tBase) :: tExtended
        LOGICAL(k) :: visited
        CLASS(tBase(l,k)), POINTER :: extendedPtr
    END TYPE tExtended

END MODULE tExtendedMod


MODULE tContainerMod
    USE tExtendedMod

    IMPLICIT NONE

    TYPE tContainer(k,l)
        INTEGER, KIND :: k
        INTEGER, LEN :: l

        CHARACTER(2) :: id
        CLASS(tBase(l,k)), ALLOCATABLE :: contents
    END TYPE tContainer

    CONTAINS

        SUBROUTINE InitializeBase(b, l, c)
            TYPE(tBase(*,2)) :: b
            CHARACTER(2) :: l
            TYPE(tContainer(2,*)), TARGET :: c

            b%label = l
            b%basePtr => c
            b%visitList = .FALSE._2

        END SUBROUTINE InitializeBase

        SUBROUTINE InitializeExtended(e, l, c, b, rc)
            CLASS(tBase(*,2)) :: e
            CHARACTER(2) :: l
            TYPE(tContainer(2,*)), TARGET :: c
            CLASS(tBase(*,2)), TARGET :: b
            INTEGER(4) :: rc

            SELECT TYPE ( e )
                TYPE IS (tExtended(*,2))
                    e%visited = .FALSE._2

                    SELECT TYPE ( b )
                        TYPE IS (tBase(*,2))
                            e%extendedPtr => b

                        TYPE IS (tExtended(*,2))
                            e%extendedPtr => b

                        CLASS DEFAULT
                            CALL zzrc( rc )
                    END SELECT

                    CALL InitializeBase(e%tBase, l, c)

                CLASS DEFAULT
                    CALL zzrc( (rc + 1_4) )
            END SELECT

        END SUBROUTINE InitializeExtended

END MODULE tContainerMod


PROGRAM componentSelector06
    USE tContainerMod

    IMPLICIT NONE

    INTERFACE
        RECURSIVE SUBROUTINE Traverse(node, rc)
            USE tContainerMod
            IMPLICIT NONE
            CLASS(*) :: node
            INTEGER(4) :: rc
        END SUBROUTINE Traverse
    END INTERFACE

    TYPE(tContainer(2,4)), TARGET :: c1, c2, c3, c4


    !
    !  Base Node (C1 -> *b1* -> C2)
    !
    c1%id = 'C1'
    ALLOCATE( tBase(4,2) :: c1%contents )
    CALL InitializeBase(c1%contents, 'b1', c2)

    !
    !  Extended Node (C1 -> b1 -> C2 -> *e1* -> C3)
    !  - Cycle: e1 -+
    !           ^   |
    !           +---+
    !
    c2%id = 'C2'
    ALLOCATE( tExtended(4,2) :: c2%contents )
    CALL InitializeExtended(c2%contents, 'e1', c3, c2%contents, 5_4)

    !
    !  Extended Node (C1 -> b1 -> C2 -> e1 -> C3 -> *e2* -> C4)
    !  - Backwards Cycle:  e1 --> C2 --> e2
    !                       ^             |
    !                       +-------------+
    !
    c3%id = 'C3'
    ALLOCATE( tExtended(4,2) :: c3%contents )
    CALL InitializeExtended(c3%contents, 'e2', c4, c2%contents, 10_4)

    !
    !  Extended Node (C1 -> b1 -> C2 -> e1 -> C3 -> e2 -> C4 -> *e3* -> C1)
    !  - Backwards Cycle:  b1 -> C2 -> ... -> C4 -> e3
    !                      ^                        |
    !                      +------------------------+
    !
    c4%id = 'C4'
    ALLOCATE( tExtended(4,2) :: c4%contents )
    CALL InitializeExtended(c4%contents, 'e3', c1, c1%contents, 15_4)


    CALL DumpBase(c1%contents, c2, 200_4)

    CALL DumpExtended(c2%contents, c3, c2%contents, 205_4)
    CALL DumpExtended(c3%contents, c4, c2%contents, 210_4)
    CALL DumpExtended(c4%contents, c1, c1%contents, 215_4)

    PRINT *


    !
    !  Recursively traverse the Nodes (as listed above):
    !
    !  C[1-4] - Proceed to the next node
    !  b1     - Proceed to the next node
    !  e[1-3] - Traverse the Alternate Path (as indicated above), then
    !           proceed to the next node
    !
    !  Recursion terminates when:  b1 and e[1-3] have been visited more
    !  than 4 times (refer to the .vf file for the traversal).
    !
    CALL Traverse(c1, 20_4)

    PRINT *


    CALL DumpBase(c1%contents, c2, 200_4)

    CALL DumpExtended(c2%contents, c3, c2%contents, 205_4)
    CALL DumpExtended(c3%contents, c4, c2%contents, 210_4)
    CALL DumpExtended(c4%contents, c1, c1%contents, 215_4)


    CONTAINS

        SUBROUTINE DumpBase(b, next, rc)
            CLASS(tBase(*,2)), ALLOCATABLE :: b
            CLASS(*), TARGET :: next
            INTEGER(4) :: rc

            SELECT TYPE ( b )
                TYPE IS (tBase(*,2))
                    PRINT *, b%label, ' (', b%visitList, ')',&
                                    ASSOCIATED(b%basePtr, next)

                CLASS DEFAULT
                    CALL zzrc( rc )
            END SELECT

        END SUBROUTINE DumpBase

        SUBROUTINE DumpExtended(e, next, nextExt, rc)
            CLASS(tBase(*,2)), ALLOCATABLE :: e
            CLASS(*), TARGET :: next
            CLASS(tBase(*,2)), TARGET :: nextExt
            INTEGER(4) :: rc

            SELECT TYPE ( e )
                TYPE IS (tExtended(*,2))
                    PRINT *, e%label, ' (', e%visitList, ')',&
                            ASSOCIATED(e%basePtr, next), '--',&
                            e%visited, ASSOCIATED(e%extendedPtr, nextExt)

                CLASS DEFAULT
                    CALL zzrc( rc )
            END SELECT

        END SUBROUTINE DumpExtended

END PROGRAM componentSelector06


RECURSIVE SUBROUTINE Traverse(thisnode, rc)
    USE tContainerMod

    IMPLICIT NONE

    CLASS(*) :: thisnode
    INTEGER(4) :: rc

    INTEGER :: numVisits


        SELECT TYPE ( thisNode )
            TYPE IS (tContainer(2,*))
                PRINT *, thisNode%id, rc

                    CALL Traverse(thisNode%contents, (rc + 5_4))

            TYPE IS (tBase(*,2))
                PRINT *, thisNode%label, rc
                    numVisits = MarkNodeVisited( thisNode%visitList )
                    IF (numVisits <= 4) THEN
                        CALL Traverse(thisNode%basePtr, (rc + 5_4))
                    END IF


            TYPE IS (tExtended(*,2))
                PRINT *, thisNode%label, rc

                    numVisits = MarkNodeVisited( thisNode%visitList )

                    IF (.NOT. thisNode%visited) THEN
                        thisNode%visited = .TRUE._2
                        CALL Traverse(thisNode%extendedPtr, (rc + 5_4))

                        PRINT *
                    END IF

                    IF (numVisits <= 4) THEN
                        CALL Traverse(thisNode%basePtr, (rc + 5_4))
                    END IF

            CLASS DEFAULT
                CALL zzrc( rc )
        END SELECT

    CONTAINS

        INTEGER FUNCTION MarkNodeVisited( nodeArray )
            LOGICAL(2) :: nodeArray( : )

            MarkNodeVisited = 0
            DO WHILE (( nodeArray( MarkNodeVisited ) )  .AND.&
                        (MarkNodeVisited <= SIZE( nodeArray )))
                MarkNodeVisited = MarkNodeVisited + 1
            END DO

            IF (MarkNodeVisited <= 4) THEN
                nodeArray( MarkNodeVisited ) = .TRUE._2
            END IF

        END FUNCTION MarkNodeVisited

END SUBROUTINE Traverse
