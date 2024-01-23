!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : October 22, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DIRECT File Input/Output of Dervied Type
!*                               Containers (with Parameters)
!*  SECONDARY FUNCTIONS TESTED : I/O is performed using Type-Bound Procedures
!*                               (with a PASSed argument)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE, READ
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To perform an Unformatted WRITE of an array of Derive Type to a
!*  Direct file, READ the the data written, and confirm the contents.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE tCoordMod
    IMPLICIT NONE

    TYPE tCoord(l1,k1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        COMPLEX(k1) :: coord( l1 )
    END TYPE tCoord

END MODULE tCoordMod


MODULE tShapeMod
    USE tCoordMod

    IMPLICIT NONE

    TYPE tShape(l1,l2,k1)
        INTEGER, LEN :: l1
        INTEGER, LEN :: l2
        INTEGER, KIND :: k1

        COMPLEX(k1) :: values( (l1 + 1) )
        TYPE(tCoord((l1 * l2),k1)) :: coord
    END TYPE tShape

END MODULE tShapeMod


MODULE tTriangleMod
    USE tShapeMod

    IMPLICIT NONE

    TYPE tTriangle(l1,k1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        TYPE(tShape(l1,3,k1)) :: coord

        CONTAINS

            PROCEDURE, PASS :: TriangleRead
            PROCEDURE, PASS :: TriangleWrite

    END TYPE tTriangle


    CONTAINS

        INTEGER FUNCTION TriangleRead(this, u, n)
            IMPLICIT NONE

            CLASS(tTriangle(*,8)) :: this
            INTEGER :: u
            INTEGER :: n

            CHARACTER(255) :: iomsg = 'Unknown Type'


            SELECT TYPE (this)
                TYPE IS (tTriangle(*,8))
                READ(u, REC=n, IOSTAT=TriangleRead, IOMSG=iomsg) this

                CLASS DEFAULT
                    TriangleRead = 1
            END SELECT

            IF (TriangleRead /= 0) THEN
                PRINT *, "TriangleRead(", u, ",", n,&
                            ") (", TriangleRead, ") ", iomsg
            END IF

        END FUNCTION TriangleRead

        INTEGER FUNCTION TriangleWrite(this, u, n)
            IMPLICIT NONE

            CLASS(tTriangle(*,8)) :: this
            INTEGER :: u
            INTEGER :: n

            CHARACTER(255) :: iomsg = 'Unknown Type'


            SELECT TYPE (this)
                TYPE IS (tTriangle(*,8))
                    WRITE(u, REC=n, IOSTAT=TriangleWrite, IOMSG=iomsg) this

                CLASS DEFAULT
                    TriangleWrite = 1
            END SELECT

            IF (TriangleWrite /= 0) THEN
                PRINT *, "TriangleWrite(", u, ",", n,&
                            ") (", TriangleWrite, ") ", iomsg
            END IF

        END FUNCTION TriangleWrite

END MODULE tTriangleMod


PROGRAM directReadWrite07
    USE tTriangleMod

    IMPLICIT NONE

    INTERFACE
        SUBROUTINE Dump(u, t, o, rc)
            USE tTriangleMod

            IMPLICIT NONE

            INTEGER :: u
            TYPE(tTriangle(*,8)) :: t( : )
            INTEGER :: o( : )
            INTEGER(4) :: rc
        END SUBROUTINE Dump

        SUBROUTINE Grab(u, t, o, rc)
            USE tTriangleMod

            IMPLICIT NONE

            INTEGER :: u
            TYPE(tTriangle(*,8)) :: t( : )
            INTEGER :: o( : )
            INTEGER(4) :: rc
        END SUBROUTINE Grab
    END INTERFACE

    INTEGER :: i
    INTEGER :: j
    INTEGER :: iostat
    INTEGER :: ioOrder( 5 ) = [ 5, 3, 1, 2, 4 ]

    COMPLEX(8) :: initialData( 50 )

    CHARACTER(255) :: iomsg

    TYPE(tTriangle(2,8)) :: triangleIn( 5 )
    TYPE(tTriangle(2,8)) :: triangleOut( 5 )


    DO i = 1, 100, 2
        initialData( i/2+1 ) =&
            CMPLX((1.0_8 / REAL(i, 8)), (1.0_8 / REAL((i + 1), 8)), 8)
    END DO

    DO i = 1, 5
        j = ((i - 1) * 10) + 1
        triangleOut( i )%coord%values = initialData( j:(j + 2) )
        triangleOut( i )%coord%coord%coord = initialData( (j + 4):(j + 9) )
    END DO


    OPEN(9, ACCESS='direct', ACTION='readwrite',&
            RECL=160, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "OPEN(", iostat, ") ", iomsg
        ERROR STOP 10_4
    END IF


    CALL Dump(9, triangleOut, ioOrder, 20_4)
    CALL Grab(9, triangleIn, ioOrder, 30_4)

    DO i = 1, 5
        j = 6 - i

        IF ( ANY(triangleIn( j )%coord%values&
                    /= triangleOut( i )%coord%values) )&
                            CALL zzrc( (40_4 + INT(i, 4)) )

        IF ( ANY(triangleIn( j )%coord%coord%coord&
                    /= triangleOut( i )%coord%coord%coord) )&
                                CALL zzrc( (50_4 + INT(i, 4)) )
    END DO


    CLOSE(9, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "CLOSE(", iostat, ") ", iomsg
        ERROR STOP 60_4
    END IF

END PROGRAM directReadWrite07


SUBROUTINE Dump(u, t, o, rc)
    USE tTriangleMod

    IMPLICIT NONE

    INTEGER :: u
    TYPE(tTriangle(*,8)) :: t( : )
    INTEGER :: o( : )
    INTEGER(4) :: rc

    INTEGER(4) :: i
    INTEGER :: iostat


    IF (SIZE( t ) /= SIZE( o )) CALL zzrc( rc )

    DO i = 1_4, SIZE( t )
        iostat = t( i )%TriangleWrite(u, o( i ))
        IF (iostat /= 0) CALL zzrc( (rc + i) )
    END DO

END SUBROUTINE Dump


SUBROUTINE Grab(u, t, o, rc)
    USE tTriangleMod

    IMPLICIT NONE

    INTEGER :: u
    TYPE(tTriangle(*,8)) :: t( : )
    INTEGER :: o( : )
    INTEGER(4) :: rc

    INTEGER(4) :: i
    INTEGER :: iostat


    IF (SIZE( t ) /= SIZE( o )) CALL zzrc( rc )

    DO i = 1_4, SIZE( t )
        iostat = t( i )%TriangleRead(u, o( (SIZE( o ) - i + 1_4) ))
        IF (iostat /= 0) CALL zzrc( (rc + i) )
    END DO

END SUBROUTINE Grab
