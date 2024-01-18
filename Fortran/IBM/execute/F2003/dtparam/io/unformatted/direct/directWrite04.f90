!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : October  6, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : output-item-list consists of Derived Types
!*                               (with Parameters) that are Containers for
!*                               other Derived Types (also with Type Parameters)
!*  SECONDARY FUNCTIONS TESTED : Length Paramters involve simple Expressions
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To perform Unformatted Direct Output of Derived Types within up to 4
!*  Levels of Container Derived Types.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE c1
    IMPLICIT NONE

    TYPE tC1(l1,k,l2)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k
        INTEGER, LEN :: l2

        INTEGER(k) :: i( l1 )
        INTEGER(k) :: f( l2 )
    END TYPE tC1

END MODULE c1


MODULE c2
    USE c1

    IMPLICIT NONE

    TYPE tC2(l1,k,l2)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k
        INTEGER, LEN :: l2

        TYPE(tC1((l1 - 1),k,(l2 + 1))) :: c
    END TYPE tC2

END MODULE c2


MODULE c3
    USE c2

    IMPLICIT NONE

    TYPE tC3(l1,k,l2)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k
        INTEGER, LEN :: l2

        TYPE(tC2((l1 - 1),k,(l2 + 1))) :: c
    END TYPE tC3

END MODULE c3


MODULE c4
    USE c3

    IMPLICIT NONE

    TYPE tC4(l1,k,l2)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k
        INTEGER, LEN :: l2

        TYPE(tC3((l1 - 1),k,(l2 + 1))) :: c
    END TYPE tC4

END MODULE c4


PROGRAM directWrite04
    USE c4

    IMPLICIT NONE

    INTEGER(2) :: i
    INTEGER(4) :: j

    INTEGER :: iostat

    CHARACTER(255) :: iomsg

    TYPE(tC1(8,2,0)) :: vC1
    TYPE(tC2(4,4,0)) :: vC2
    TYPE(tC3(2,8,0)) :: vC3
    TYPE(tC4(4,2,4)) :: vC4


    vC1 = tC1(8,2,0)([ (i, i = 1_2, 8_2) ],[ INTEGER(2) :: ])
    vC2 = tC2(4,4,0)(tC1(3,4,1)([ (j, j = 1_4, 3_4) ],[ -1_4 ]))
    vC3 = tC3(2,8,0)(tC2(1,8,1)(tC1(0,8,2)([ INTEGER(8) :: ],[ -1_8, -1_8 ])))
    vC4 = tC4(4,2,4)(tC3(3,2,5)(tC2(2,2,6)(&
                        tC1(1,2,7)([ 8_2 ],[ (-1_2, j = 1_4, 7_4) ]))))


    OPEN(14, ACCESS='direct', ACTION='write',&
            RECL=16, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'OPEN(', iostat, '): ', iomsg
        STOP 10
    END IF


    WRITE(14, REC=3, IOSTAT=iostat, IOMSG=iomsg) vC2
    IF (iostat /= 0) THEN
        PRINT *, 3, 'WRITE(', iostat, '): ', iomsg
        STOP 20
    END IF

    WRITE(14, REC=1, IOSTAT=iostat, IOMSG=iomsg) vC4
    IF (iostat /= 0) THEN
        PRINT *, 1, 'WRITE(', iostat, '): ', iomsg
        STOP 30
    END IF

    WRITE(14, REC=2, IOSTAT=iostat, IOMSG=iomsg) vC3
    IF (iostat /= 0) THEN
        PRINT *, 2, 'WRITE(', iostat, '): ', iomsg
        STOP 40
    END IF

    WRITE(14, REC=4, IOSTAT=iostat, IOMSG=iomsg) vC1
    IF (iostat /= 0) THEN
        PRINT *, 4, 'WRITE(', iostat, '): ', iomsg
        STOP 50
    END IF


    CLOSE(14, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'CLOSE(', iostat, '): ', iomsg
        STOP 60
    END IF

END PROGRAM directWrite04
