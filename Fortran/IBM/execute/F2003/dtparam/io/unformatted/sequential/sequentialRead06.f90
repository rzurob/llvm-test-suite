!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : sequentialRead06
!*
!*  DATE                       : October 31, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : SEQUENTIAL File Input for a Derived/Extended
!*                               Derived Type (with Type Parameters)
!*  SECONDARY FUNCTIONS TESTED : Derived Types are ALLOCATED Allocatables
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : READ
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To perform an Unformatted READ of a Derive Type/Extended Derived Type
!*  from a Sequential file, and confirm the contents.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

PROGRAM sequentialRead06

    IMPLICIT NONE

    TYPE tN
        CHARACTER(6) :: c
    END TYPE tN

    TYPE, EXTENDS(tN) :: eN
        INTEGER(4) :: i( 3,6 )
    END TYPE eN

    TYPE tP(l1,k1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        CHARACTER((l1 * 2)) :: c
    END TYPE tP

    TYPE, EXTENDS(tP) :: eP
        INTEGER(k1) :: i( l1,(l1 * 2) )
    END TYPE eP


    INTEGER :: i
    INTEGER :: j
    INTEGER :: k

    INTEGER :: iostat

    CHARACTER(255) :: iomsg

    CHARACTER(6) :: str( 3 ) = [ 'Hello!', 'to the', 'World!' ]

    INTEGER :: ioLength( 3 )
    INTEGER(4) :: array( 3,6 ) = RESHAPE([ (i, i = 1, 18) ], [ 3,6 ])

    TYPE(tN) :: tOut( 3 )
    TYPE(tP(:,4)), ALLOCATABLE :: tIn( : )

    TYPE(eN) :: eOut( 3 )
    TYPE(eP(:,4)), ALLOCATABLE :: eIn( : )


    tOut = [ (tN(str( i )), i = 1, 3) ]
    eOut = [ (eN(str( i ),array), i = 1, 3) ]


    OPEN(41, ACCESS='sequential', ACTION='write',&
            FORM='unformatted', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'OPEN(', iostat, ') ', iomsg
        CALL zzrc( 10_4 )
    END IF


    WRITE(41, IOSTAT=iostat, IOMSG=iomsg) tOut( 1 )
    IF (iostat /= 0) THEN
        PRINT *, 'WRITE(', iostat, ') ', iomsg
        CALL zzrc( 20_4 )
    END IF

    DO i = 1, 3
        WRITE(41, IOSTAT=iostat, IOMSG=iomsg) eOut( i )
        IF (iostat /= 0) THEN
            PRINT *, 'WRITE(', iostat, ') ', iomsg
            CALL zzrc( (30_4 + INT(i, 4)) )
        END IF
    END DO

    WRITE(41, IOSTAT=iostat, IOMSG=iomsg) tOut( 2:3 )
    IF (iostat /= 0) THEN
        PRINT *, 'WRITE(', iostat, ') ', iomsg
        CALL zzrc( 40_4 )
    END IF


    CLOSE(41, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'CLOSE(', iostat, ') ', iomsg
        CALL zzrc( 50_4 )
    END IF


    ALLOCATE(tP(3,4) :: tIn( 3 ))
    ALLOCATE(eP(3,4) :: eIn( 3 ))


    OPEN(41, ACCESS='sequential', ACTION='read',&
            FORM='unformatted', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'OPEN(', iostat, ') ', iomsg
        CALL zzrc( 60_4 )
    END IF


    READ(41, IOSTAT=iostat, IOMSG=iomsg) tIn( 1 )
    IF (iostat /= 0) THEN
        PRINT *, 'READ(', iostat, ') ', iomsg
        CALL zzrc( 70_4 )
    END IF

    DO i = 1, 3
        INQUIRE( IOLENGTH=ioLength( i ) ) eIn( i )

        READ(41, IOSTAT=iostat, IOMSG=iomsg) eIn( i )
        IF (iostat /= 0) THEN
            PRINT *, 'READ(', iostat, ') ', iomsg
            CALL zzrc( (80_4 + INT(i, 4)) )
        END IF
    END DO

    READ(41, IOSTAT=iostat, IOMSG=iomsg) tIn( 2:3 )
    IF (iostat /= 0) THEN
        PRINT *, 'READ(', iostat, ') ', iomsg
        CALL zzrc( 90_4 )
    END IF


    CLOSE(41, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'CLOSE(', iostat, ') ', iomsg
        CALL zzrc( 100_4 )
    END IF


    DO i = 1, 3
        PRINT *
        PRINT *, "tIn(", i, ")%c = '", tIn( i )%c, "' (", str( i ), ")"
        IF (tIn( i )%c /= str( i ))         CALL zzrc( (110_4 + INT(i, 4)) )

        PRINT *
        PRINT *, "eIn(", i, ")%c = '", eIn( i )%c, "' (", str( i ), ")"
        PRINT *, "eIn(", i, ")%i = [", eIn( i )%i, "]"
        PRINT *, "array = [", array, "]"
        IF (eIn( i )%c /= str( i ))         CALL zzrc( (120_4 + INT(i, 4)) )
        IF ( ANY(eIn( i )%i /= array) )     CALL zzrc( (130_4 + INT(i, 4)) )

        PRINT *
        PRINT *, 'ioLength(', i, ') =', ioLength( i )
        IF (ioLength( i ) /= 80)            CALL zzrc( (140_4 + INT(i, 4)) )
    END DO

END PROGRAM sequentialRead06
