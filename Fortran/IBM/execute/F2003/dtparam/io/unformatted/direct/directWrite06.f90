!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : directWrite06
!*  TEST CASE TITLE            : Unformatted Intrinsic Input/Output (with DTP)
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : November  3, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DIRECT File Output for a Derived/Extended
!*                               Derived Type (with Type Parameters)
!*  SECONDARY FUNCTIONS TESTED : Derived Types are Polymorphic ALLOCATED
!*                               Allocatables
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To perform an Unformatted WRITE of a Polymorphic Derived Type/Extended
!*  Derived Type to a Direct file, and confirm the contents.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE nonDTP

    IMPLICIT NONE

    TYPE tN
        INTEGER(8) :: i
    END TYPE tN

    TYPE, EXTENDS(tN) :: eN
        REAL(4) :: r( 6,3 )
    END TYPE eN

    TYPE(tN) :: baseIn( 10 )
    TYPE(eN) :: extendedBaseIn( 2 )
    TYPE(eN) :: extendedIn( 2 )

END MODULE nonDTP

MODULE dtp

    IMPLICIT NONE

    TYPE tP(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        INTEGER(k1) :: i
    END TYPE tP

    TYPE, EXTENDS(tP) :: eP(k2)
        INTEGER, KIND :: k2
        REAL(k2) :: r( (l1 * 2),l1 )
    END TYPE eP

    CLASS(tP(8,:)), ALLOCATABLE :: baseOut( : )
    CLASS(tP(8,:)), ALLOCATABLE :: extendedBaseOut( : )
    CLASS(eP(8,:,4)), ALLOCATABLE :: extendedOut( : )

END MODULE dtp

MODULE baseData

    IMPLICIT NONE

    INTEGER :: orderIdx
    INTEGER :: order( 5 )

    INTEGER(8) :: iArray( 14 )
    REAL(4) :: rArray( 6,3 )

END MODULE baseData

PROGRAM directWrite06
    USE baseData

    IMPLICIT NONE

    INTERFACE
        SUBROUTINE CreateFile(file, rc)
            USE baseData
            USE dtp
            IMPLICIT NONE
            CHARACTER(*) :: file
            INTEGER(4) :: rc
        END SUBROUTINE CreateFile

        SUBROUTINE ReadFile(file, rc)
            USE baseData
            USE nonDTP
            IMPLICIT NONE
            CHARACTER(*) :: file
            INTEGER(4) :: rc
        END SUBROUTINE ReadFile

        SUBROUTINE VerifyData( rc )
            USE nonDTP
            USE baseData
            IMPLICIT NONE
            INTEGER(4) :: rc
        END SUBROUTINE VerifyData
    END INTERFACE

    INTEGER :: i

    INTEGER(4) :: rc

    CHARACTER(17) :: file = 'directWrite06.dat'


    orderIdx = 1
    order = [ 3, 1, 4, 2, 5 ]

    iArray = [ (INT(i, 8), i = 99, 86, -1) ]
    rArray = RESHAPE([ ((1.0_4 / REAL(i, 4)), i = 1, 18) ], [ 6,3 ])

    CALL CreateFile(file, 10_4)
    CALL ReadFile(file, 30_4)
    CALL VerifyData( 40_4 )

END PROGRAM directWrite06


SUBROUTINE CreateFile(file, rc)
    USE baseData
    USE dtp

    IMPLICIT NONE

    CHARACTER(*) :: file
    INTEGER(4) :: rc

    INTEGER :: i
    INTEGER :: iostat
    CHARACTER(255) :: iomsg


    OPEN(41, FILE=file, ACCESS='direct', ACTION='write',&
         RECL=80, FORM='unformatted', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'OPEN(', iostat, ') ', iomsg
        CALL zzrc( (rc + 1_4) )
    END IF


    ALLOCATE(baseOut( 10 ), SOURCE=[ (tP(8,5)(iArray( i )), i = 1, 10) ])

    SELECT TYPE (baseOut)
        TYPE IS (tP(8,*))
            WRITE(41, REC=order( orderIdx ),&
                 IOSTAT=iostat, IOMSG=iomsg)&
                 (baseOut( i ), i = 1, SIZE( baseOut ))

        CLASS DEFAULT
            CALL zzrc( (rc + 2_4) )
    END SELECT

    IF (iostat /= 0) THEN
        PRINT *, 'WRITE(REC=', order( orderIdx ), ',', iostat, ') ', iomsg
        CALL zzrc( (rc + 3_4) )
    END IF

    orderIdx = orderIdx + 1


    ALLOCATE(eP(8,3,4) :: extendedBaseOut( 2 ))

    SELECT TYPE (extendedBaseOut)
        TYPE IS (eP(8,*,4))
            DO i = 1, 2
                extendedBaseOut( i )%i = iArray( (10 + i) )
                extendedBaseOut( i )%r = rArray

                WRITE(41, REC=order( orderIdx ),&
                      IOSTAT=iostat, IOMSG=iomsg) extendedBaseOut( i )
                IF (iostat /= 0) THEN
                    PRINT *, 'WRITE(REC=', order( orderIdx ),&
                                    ',', iostat, ') ', iomsg
                    CALL zzrc( (rc + 4_4 + INT(i, 4)) )
                END IF

                orderIdx = orderIdx + 1
            END DO

        CLASS DEFAULT
            CALL zzrc( (rc + 4_4) )
    END SELECT


    ALLOCATE(extendedOut( 2 ),&
             SOURCE=[ (eP(8,3,4)(iArray( i ),rArray), i = 13, 14) ])
    SELECT TYPE (extendedOut)
        TYPE IS (eP(8,*,4))
            DO i = 1, 2
                PRINT *, "extendedOut(", i, ")%i =", extendedOut( i )%i
                WRITE(41, REC=order( orderIdx ),&
                      IOSTAT=iostat, IOMSG=iomsg) extendedOut( i )
                IF (iostat /= 0) THEN
                    PRINT *, 'WRITE(REC=', order( orderIdx ),&
                                    ',', iostat, ') ', iomsg
                    CALL zzrc( (rc + 7_4 + INT(i, 4)) )
                END IF

                orderIdx = orderIdx + 1
            END DO

        CLASS DEFAULT
            CALL zzrc( (rc + 7_4) )
    END SELECT


    CLOSE(41, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'CLOSE(', iostat, ') ', iomsg
        CALL zzrc( (rc + 10_4) )
    END IF

END SUBROUTINE CreateFile


SUBROUTINE ReadFile(file, rc)
    USE baseData
    USE nonDTP

    IMPLICIT NONE

    CHARACTER(*) :: file
    INTEGER(4) :: rc

    INTEGER :: i
    INTEGER :: iostat
    CHARACTER(255) :: iomsg


    OPEN(41, ACCESS='direct', RECL=80, ACTION='read',&
         FILE=file, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'OPEN(', iostat, ') ', iomsg
        CALL zzrc( (rc + 1_4) )
    END IF


    DO i = 2, 1, -1
        orderIdx = orderIdx - 1
        READ(41, REC=order( orderIdx ),&
             IOSTAT=iostat, IOMSG=iomsg) extendedIn( i )
        IF (iostat /= 0) THEN
            PRINT *, 'READ(REC=', order( orderIdx ), ',', iostat, ') ', iomsg
            CALL zzrc( (rc + 1_4 + INT(i, 4)) )
        END IF
    END DO


    DO i = 2, 1, -1
        orderIdx = orderIdx - 1
        READ(41, REC=order( orderIdx ),&
             IOSTAT=iostat, IOMSG=iomsg) extendedBaseIn( i )
        IF (iostat /= 0) THEN
            PRINT *, 'READ(REC=', order( orderIdx ), ',', iostat, ') ', iomsg
            CALL zzrc( (rc + 3_4 + INT(i, 4)) )
        END IF
    END DO


    orderIdx = orderIdx - 1
    READ(41, REC=order( orderIdx ), IOSTAT=iostat, IOMSG=iomsg) baseIn
    IF (iostat /= 0) THEN
        PRINT *, 'READ(REC=', order( orderIdx ), ',', iostat, ') ', iomsg
        CALL zzrc( (rc + 6_4) )
    END IF


    CLOSE(41, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'CLOSE(', iostat, ') ', iomsg
        CALL zzrc( (rc + 7_4) )
    END IF

END SUBROUTINE ReadFile


SUBROUTINE VerifyData( rc )
    USE nonDTP
    USE baseData

    IMPLICIT NONE

    INTEGER(4) :: rc

    INTEGER :: i


    IF ( ANY(baseIn%i /= iArray( 1:10 )) ) THEN
        CALL zzrc( (rc + 1_4) )
    END IF

    DO i = 1, 2
        IF (extendedBaseIn( i )%i /= iArray( (10 + i) )) THEN
            CALL zzrc( (rc + 10_4 + INT(i, 4)) )

        ELSE IF ( ANY(extendedBaseIn( i )%r /= rArray) ) THEN
            CALL zzrc( (rc + 20_4 + INT(i, 4)) )

        ELSE IF (extendedIn( i )%i /= iArray( (12 + i) )) THEN
            CALL zzrc( (rc + 30_4 + INT(i, 4)) )

        ELSE IF ( ANY(extendedIn( i )%r /= rArray) ) THEN
            CALL zzrc( (rc + 40_4 + INT(i, 4)) )
        END IF
    END DO

END SUBROUTINE VerifyData
