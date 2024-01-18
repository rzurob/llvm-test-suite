!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : October 22, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SEQUENTIAL File Input for a Derived/Extended
!*                               Derived Type (with Type Parameters)
!*  SECONDARY FUNCTIONS TESTED : Derived Types are SEQUENCE Types
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE, READ
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To perform an Unformatted READ of a SEQUENCE Derive Type from a
!*  Sequential file, and confirm the contents.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE tSeq1Mod
    IMPLICIT NONE

    TYPE tSeq1(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        SEQUENCE

        INTEGER(k1) :: t
        CHARACTER(l1) :: n
    ENDTYPE tSeq1

END MODULE tSeq1Mod


MODULE tSeq2Mod
    USE tSeq1Mod

    IMPLICIT NONE

    TYPE tSeq2(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        SEQUENCE

        INTEGER(k1) :: t
        CHARACTER(l1 + 2) :: n
        TYPE(tSeq1(k1,(l1 * 2))) :: s1
    ENDTYPE tSeq2

END MODULE tSeq2Mod


PROGRAM sequentialRead05
    USE tSeq2Mod

    IMPLICIT NONE

    INTEGER(2) :: id( 3 ) = [ 97_2, 98_2, 99_2 ]

    INTEGER :: iostat
    INTEGER :: record1Length
    INTEGER :: record2Length

    CHARACTER(3) :: name1 = 'IBM'
    CHARACTER(5) :: name2 = 'F2003'
    CHARACTER(6) :: name3 = 'Tester'

    CHARACTER(255) :: iomsg

    TYPE(tSeq1(2,3)) :: seq1
    TYPE(tSeq2(2,3)) :: seq2


    OPEN(13, ACCESS='sequential', ACTION='write',&
         FORM='unformatted', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "OPEN(", iostat, ") ", iomsg
        STOP 10
    END IF


    ! Record 1:  6 bytes:  00 61  49 42 4d
    !                      (97, 'IBM')
    WRITE(13, IOSTAT=iostat, IOMSG=iomsg) id( 1 ), name1
    IF (iostat /= 0) THEN
        PRINT *, "WRITE(", iostat, ") ", iomsg
        STOP 20
    END IF

    ! Record 2:  16 bytes:  00 62  46 32 30 30 33  00 63  54 65 73 74 65 72
    !                       (98, 'F2003', 99, 'Tester')
    WRITE(13, IOSTAT=iostat, IOMSG=iomsg) id( 2 ), name2, id( 3 ), name3
    IF (iostat /= 0) THEN
        PRINT *, "WRITE(", iostat, ") ", iomsg
        STOP 21
    END IF

    WRITE(13, IOSTAT=iostat, IOMSG=iomsg) -1_8
    IF (iostat /= 0) THEN
        PRINT *, "WRITE(", iostat, ") ", iomsg
        STOP 22
    END IF


    CLOSE(13, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "CLOSE(", iostat, ") ", iomsg
        STOP 30
    END IF


    OPEN(13, ACCESS='sequential', ACTION='read',&
         FORM='unformatted', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "OPEN(", iostat, ") ", iomsg
        STOP 40
    END IF


    INQUIRE( IOLENGTH=record1Length ) seq1
    PRINT *, "record1Length =", record1Length
    IF (record1Length /= 5) THEN
        STOP 50
    END IF

    READ(13, IOSTAT=iostat, IOMSG=iomsg) seq1
    IF (iostat /= 0) THEN
        PRINT *, "READ(", iostat, ") ", iomsg
        STOP 51
    END IF


    INQUIRE( IOLENGTH=record2Length ) seq2
    PRINT *, "record2Length =", record2Length
    IF (record2Length /= 15) THEN
        STOP 55
    END IF

    READ(13, IOSTAT=iostat, IOMSG=iomsg) seq2
    IF (iostat /= 0) THEN
        PRINT *, "READ(", iostat, ") ", iomsg
        STOP 56
    END IF


    CLOSE(13, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "CLOSE(", iostat, ") ", iomsg
        STOP 60
    END IF



    PRINT *

    PRINT *, 'seq1%t = "', seq1%t, '" (', id( 1 ), ')'
    IF (seq1%t /= id( 1 ))      STOP 111
    PRINT *, 'seq1%n = "', seq1%n, '" (', name1, ')'
    IF (seq1%n /= name1)        STOP 112

    PRINT *


    PRINT *, 'seq2%t = "', seq2%t, '" (', id( 2 ), ')'
    IF (seq2%t /= id( 2 ))      STOP 121
    PRINT *, 'seq2%n = "', seq2%n, '" (', name2, ')'
    IF (seq2%n /= name2)        STOP 122

    PRINT *, 'seq2%s1%t = "', seq2%s1%t, '" (', id( 3 ), ')'
    IF (seq2%s1%t /= id( 3 ))   STOP 125
    PRINT *, 'seq2%s1%n = "', seq2%s1%n, '" (', name3, ')'
    IF (seq2%s1%n /= name3)     STOP 126

END PROGRAM sequentialRead05
