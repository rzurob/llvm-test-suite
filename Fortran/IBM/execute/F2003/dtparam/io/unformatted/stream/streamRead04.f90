!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : streamRead04
!*
!*  DATE                       : October 22, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Data Transfer of a Derived Type and a
!*                               Container Derived Type (both with Type
!*                               Parameters) from a STREAM File
!*  SECONDARY FUNCTIONS TESTED : Both Derived Types have no Components
!*
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


PROGRAM streamRead04
    USE eMod

    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: iostat
    CHARACTER(255) :: iomsg


    !
    !  Read empty file (since the types are empty, this shouldn't be an issue).
    !
    CALL ReadIt( 10_4 )


    !
    !  Add something to the file.
    !
    OPEN(7, ACCESS='stream', ACTION='write', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'OPEN(', iostat, ') ', iomsg
        CALL zzrc( 20_4 )
    END IF

    WRITE(7, IOSTAT=iostat, IOMSG=iomsg) (i, i = 1, 10)
    IF (iostat /= 0) THEN
        PRINT *, 'WRITE(', iostat, ') ', iomsg
        CALL zzrc( 21_4 )
    END IF

    CLOSE(7, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'CLOSE(', iostat, ') ', iomsg
        CALL zzrc( 22_4 )
    END IF


    !
    !  Read file again.
    !
    CALL ReadIt( 30_4 )


    CONTAINS

        SUBROUTINE ReadIt( rc )
            INTEGER(4) :: rc

            INTEGER(2) :: filler1( 512 ) = 99

            TYPE(b1(3,5)) :: b1v
            TYPE(b2(5,7)) :: b2v
            TYPE(e1(1,3,5,7)) :: e1v
            TYPE(e2(7,5,3,1)) :: e2v

            INTEGER(2) :: filler2( 512 ) = 101

            INTEGER :: iostat
            CHARACTER(255) :: iomsg


            OPEN(7, ACCESS='stream', ACTION='read', IOSTAT=iostat, IOMSG=iomsg)
            IF (iostat /= 0) THEN
                PRINT *, 'OPEN(', iostat, ') ', iomsg
                CALL zzrc( rc )
            END IF

            READ(7, IOSTAT=iostat, IOMSG=iomsg) b1v, e1v, b2v, e2v
            IF (iostat /= 0) THEN
                PRINT *, 'READ(', iostat, ') ', iomsg
                CALL zzrc( (rc + 1_4) )
            END IF

            CLOSE(7, IOSTAT=iostat, IOMSG=iomsg)
            IF (iostat /= 0) THEN
                PRINT *, 'CLOSE(', iostat, ') ', iomsg
                CALL zzrc( (rc + 2_4) )
            END IF

            IF ( ANY(filler1 /= 99) ) THEN
                CALL zzrc( (rc + 5_4) )

            ELSE IF ( ANY(filler2 /= 101) ) THEN
                CALL zzrc( (rc + 6_4) )
            END IF

        END SUBROUTINE ReadIt

END PROGRAM streamRead04
