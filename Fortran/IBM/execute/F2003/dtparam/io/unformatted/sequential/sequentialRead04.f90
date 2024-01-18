!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : sequentialRead04
!*  TEST CASE TITLE            : Unformatted Intrinsic Input (with DTP)
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October 22, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Data Transfer of a Derived Type and a
!*                               Container Derived Type (both with Type
!*                               Parameters) from a SEQUENTIAL File
!*  SECONDARY FUNCTIONS TESTED : Both Derived Types have no Components
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To read empty Derived Type values from a data file.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE bMod
    IMPLICIT NONE

    TYPE b1(l1,k1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1
    END TYPE b1

    TYPE b2(k1,l1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        TYPE(b1((l1 * 5),k1)) :: b1
    END TYPE b2

END MODULE bMod


MODULE eMod
    USE bMod

    IMPLICIT NONE

    TYPE, EXTENDS(b1) :: e1(l2,k2)
        INTEGER, LEN :: l2
        INTEGER, KIND :: k2
    END TYPE e1

    TYPE, EXTENDS(b2) :: e2(k2,l2)
        INTEGER, LEN :: l2
        INTEGER, KIND :: k2

        TYPE(e1(l1,(k1 * 2),(l2 + 6),k2)) :: e1
    END TYPE e2

END MODULE eMod


PROGRAM sequentialRead04
    USE eMod

    IMPLICIT NONE

    TYPE tEmpty
    END TYPE tEmpty

    INTEGER :: i
    INTEGER :: iostat
    CHARACTER(255) :: iomsg

    TYPE(tEmpty) :: empty


    !
    !  Read empty file (since the types are empty, this shouldn't be an issue).
    !
    CALL WriteIt(7, empty, 10_4)
    CALL ReadIt(7, 20_4)


    !
    !  Add something to a file, and READ it again..
    !
    CALL WriteIt(8, 8, 30_4)
    CALL ReadIt(8, 40_4)


    CONTAINS

        SUBROUTINE WriteIt(u, t, rc)
            INTEGER :: u
            CLASS(*) :: t
            INTEGER(4) :: rc

            INTEGER(4) :: i

            OPEN(u, ACCESS='sequential', FORM='unformatted',&
                    ACTION='write', IOSTAT=iostat, IOMSG=iomsg)
            IF (iostat /= 0) THEN
                PRINT *, 'OPEN(', u, ',', iostat, ') ', iomsg
                CALL zzrc( rc )
            END IF

            DO i = 1_4, 3_4
                SELECT TYPE( t )
                    TYPE IS (INTEGER)
                        WRITE(u, IOSTAT=iostat, IOMSG=iomsg) t

                    TYPE IS (tEmpty)
                        WRITE(u, IOSTAT=iostat, IOMSG=iomsg) t

                    CLASS DEFAULT
                        CALL zzrc( (rc + (i * 2_4) + 1) )
                END SELECT

                IF (iostat /= 0) THEN
                    PRINT *, 'WRITE(', u, ',', iostat, ') ', iomsg
                    CALL zzrc( (rc + (i * 2_4)) )
                END IF
            END DO

            CLOSE(u, IOSTAT=iostat, IOMSG=iomsg)
            IF (iostat /= 0) THEN
                PRINT *, 'CLOSE(', u, ',', iostat, ') ', iomsg
                CALL zzrc( (rc + 1_4) )
            END IF

        END SUBROUTINE WriteIt

        SUBROUTINE ReadIt(u, rc)
            INTEGER :: u
            INTEGER(4) :: rc

            INTEGER(2) :: filler1( 512 ) = 99

            TYPE(b1(3,5)) :: b1v
            TYPE(b2(5,7)) :: b2v
            TYPE(e1(1,3,5,7)) :: e1v
            TYPE(e2(7,5,3,1)) :: e2v

            INTEGER(2) :: filler2( 512 ) = 101

            INTEGER :: iostat
            CHARACTER(255) :: iomsg


            OPEN(u, ACCESS='sequential', ACTION='read',&
                 FORM='unformatted', IOSTAT=iostat, IOMSG=iomsg)
            IF (iostat /= 0) THEN
                PRINT *, 'OPEN(', u, ',', iostat, ') ', iomsg
                CALL zzrc( rc )
            END IF

            READ(u, IOSTAT=iostat, IOMSG=iomsg) b1v, e1v
            IF (iostat /= 0) THEN
                PRINT *, 'READ(', u, ',', iostat, ') ', iomsg
                CALL zzrc( (rc + 1_4) )
            END IF

            READ(u, IOSTAT=iostat, IOMSG=iomsg) b2v
            IF (iostat /= 0) THEN
                PRINT *, 'READ(', u, ',', iostat, ') ', iomsg
                CALL zzrc( (rc + 2_4) )
            END IF

            READ(u, IOSTAT=iostat, IOMSG=iomsg) e2v
            IF (iostat /= 0) THEN
                PRINT *, 'READ(', u, ',', iostat, ') ', iomsg
                CALL zzrc( (rc + 3_4) )
            END IF

            CLOSE(u, IOSTAT=iostat, IOMSG=iomsg)
            IF (iostat /= 0) THEN
                PRINT *, 'CLOSE(', u, ',', iostat, ') ', iomsg
                CALL zzrc( (rc + 4_4) )
            END IF

            IF ( ANY(filler1 /= 99) ) THEN
                CALL zzrc( (rc + 5_4) )

            ELSE IF ( ANY(filler2 /= 101) ) THEN
                CALL zzrc( (rc + 6_4) )
            END IF

        END SUBROUTINE ReadIt

END PROGRAM sequentialRead04
