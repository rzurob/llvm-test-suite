!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : sequentialRead07
!*
!*  DATE                       : November  4, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : SEQUENTIAL File Input for a Derived/Extended
!*                               Derived Types (with Type Parameters)
!*  SECONDARY FUNCTIONS TESTED : Derived Types are Polymorphic POINTERS
!*                               ASSOCIATED with Targets
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To perform an Unformatted READ of a Polymorphic Derived Type POINTER
!*  (as a POINTER to both Base and Extended Types) as well as a Polymorphic
!*  Extended Derived Type from a Sequential file, and confirm the contents.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

PROGRAM sequentialRead07

    IMPLICIT NONE

    TYPE tN
        REAL(8) :: rb( 3 )
    END TYPE tN

    TYPE, EXTENDS(tN) :: eN
        REAL(8) :: re( 6 )
    END TYPE eN

    TYPE tP(l1,k1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        REAL(k1) :: rb( (l1 / 2) )
    END TYPE tP

    TYPE, EXTENDS(tP) :: eP
        REAL(k1) :: re( l1 )
    END TYPE eP


    INTEGER :: i
    INTEGER :: j
    INTEGER :: k

    INTEGER :: iostat

    CHARACTER(255) :: iomsg

    REAL(8) :: array( 42 ) = [ ((1.0_8 / REAL(i, 8)), i = 1, 42) ]

    TYPE(tN) :: baseOut( 2 )
    TYPE(eN) :: baseExtendedOut( 2 )
    TYPE(eN) :: extendedOut( 2 )

    TYPE(tP(3,8)), TARGET :: baseTargetIn( 2 )
    CLASS(tP(:,8)), POINTER :: basePtrIn( : )

    TYPE(eP(6,8)), TARGET :: baseExtendedTargetIn( 2 )
    CLASS(tP(:,8)), POINTER :: baseExtendedPtrIn( : )

    TYPE(eP(6,8)), TARGET :: extendedTargetIn( 2 )
    CLASS(eP(:,8)), POINTER :: extendedPtrIn( : )


    j = 1
    DO i = 1, 2
        baseOut( i ) = tN(array( j:(j + 2) ))

        j = j + 3
        k = j + 3
        baseExtendedOut( i ) = eN(array( j:(j + 2) ),array( k:(k + 5) ))

        j = j + 9
        k = j + 3
        extendedOut( i ) = eN(array( j:(j + 2) ),array( k:(k + 5) ))
    END DO


    OPEN(41, ACCESS='sequential', ACTION='write',&
            FORM='unformatted', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'OPEN(', iostat, ') ', iomsg
        CALL zzrc( 10_4 )
    END IF


    DO i = 1, 2
        WRITE(41, IOSTAT=iostat, IOMSG=iomsg) baseOut( i )
        IF (iostat /= 0) THEN
            PRINT *, 'WRITE(', iostat, ') ', iomsg
            CALL zzrc( (20_4 + INT(i, 4)) )
        END IF

        WRITE(41, IOSTAT=iostat, IOMSG=iomsg) baseExtendedOut( i )
        IF (iostat /= 0) THEN
            PRINT *, 'WRITE(', iostat, ') ', iomsg
            CALL zzrc( (30_4 + INT(i, 4)) )
        END IF

        WRITE(41, IOSTAT=iostat, IOMSG=iomsg) extendedOut( i )
        IF (iostat /= 0) THEN
            PRINT *, 'WRITE(', iostat, ') ', iomsg
            CALL zzrc( (40_4 + INT(i, 4)) )
        END IF
    END DO


    CLOSE(41, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'CLOSE(', iostat, ') ', iomsg
        CALL zzrc( 50_4 )
    END IF


    basePtrIn => baseTargetIn
    baseExtendedPtrIn => baseExtendedTargetIn
    extendedPtrIn => extendedTargetIn


    OPEN(41, ACCESS='sequential', ACTION='read',&
            FORM='unformatted', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'OPEN(', iostat, ') ', iomsg
        CALL zzrc( 60_4 )
    END IF


    DO i = 1, 2
        SELECT TYPE (basePtrIn)
            TYPE IS (tP(*,8))
                READ(41, IOSTAT=iostat, IOMSG=iomsg) basePtrIn( i )
                IF (iostat /= 0) THEN
                    PRINT *, 'READ(', iostat, ') ', iomsg
                    CALL zzrc( (70_4 + INT(i, 4)) )
                END IF

            CLASS DEFAULT
                CALL zzrc( (75_4 + INT(i, 4)) )
        END SELECT

        SELECT TYPE (baseExtendedPtrIn)
            TYPE IS (eP(*,8))
                READ(41, IOSTAT=iostat, IOMSG=iomsg) baseExtendedPtrIn( i )
                IF (iostat /= 0) THEN
                    PRINT *, 'READ(', iostat, ') ', iomsg
                    CALL zzrc( (80_4 + INT(i, 4)) )
                END IF

            CLASS DEFAULT
                CALL zzrc( (85_4 + INT(i, 4)) )
        END SELECT

        SELECT TYPE (extendedPtrIn)
            TYPE IS (eP(*,8))
                READ(41, IOSTAT=iostat, IOMSG=iomsg) extendedPtrIn( i )
                IF (iostat /= 0) THEN
                    PRINT *, 'READ(', iostat, ') ', iomsg
                    CALL zzrc( (90_4 + INT(i, 4)) )
                END IF

            CLASS DEFAULT
                CALL zzrc( (95_4 + INT(i, 4)) )
        END SELECT
    END DO


    CLOSE(41, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'CLOSE(', iostat, ') ', iomsg
        CALL zzrc( 100_4 )
    END IF


    j = 1
    DO i = 1, 2
        IF ( ANY(baseTargetIn( i )%rb&
                    /= array( j:(j + 2) )) )&
                        CALL zzrc( (110_4 + INT(i, 4)) )

        j = j + 3
        k = j + 3
        IF ( ANY(baseExtendedTargetIn( i )%rb&
                    /= array( j:(j + 2) )) )&
                        CALL zzrc( (110_4 + INT(i, 4)) )
        IF ( ANY(baseExtendedTargetIn( i )%re&
                    /= array( k:(k + 5) )) )&
                        CALL zzrc( (110_4 + INT(i, 4)) )

        j = j + 9
        k = j + 3
        IF ( ANY(extendedTargetIn( i )%rb&
                    /= array( j:(j + 2) )) )&
                        CALL zzrc( (110_4 + INT(i, 4)) )
        IF ( ANY(extendedTargetIn( i )%re&
                    /= array( k:(k + 5) )) )&
                        CALL zzrc( (110_4 + INT(i, 4)) )
    END DO

END PROGRAM sequentialRead07
