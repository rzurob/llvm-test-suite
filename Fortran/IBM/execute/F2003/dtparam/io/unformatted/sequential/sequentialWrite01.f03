!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : October  2, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type (with Type Parameters) Output
!*                               to a SEQUENTIAL File
!*  SECONDARY FUNCTIONS TESTED : Output is performed by External Procedures
!*                               referenced via an INTERFACE (Procedure
!*                               selected via KIND Parameter)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To Write an array of Derived Type through an External FUNCTION defined
!*  via an INTERFACE (based on KIND).
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE tMod
    IMPLICIT NONE

    TYPE tCmpxInt(k)
        INTEGER, KIND :: k

        INTEGER(k) :: r, i
    END TYPE tCmpxInt

END MODULE tMod


PROGRAM sequentialWrite01
    USE tMod

    IMPLICIT NONE

    INTERFACE WriteCmpx
        INTEGER FUNCTION WriteCmpx2(u, c, iomsg)
            USE tMod
            INTEGER :: u
            TYPE(tCmpxInt(2)) :: c(:)
            CHARACTER(*) :: iomsg
        END FUNCTION WriteCmpx2

        INTEGER FUNCTION WriteCmpx4(u, c, iomsg)
            USE tMod
            INTEGER :: u
            TYPE(tCmpxInt(4)) :: c(:)
            CHARACTER(*) :: iomsg
        END FUNCTION WriteCmpx4

        INTEGER FUNCTION WriteCmpx8(u, c, iomsg)
            USE tMod
            INTEGER :: u
            TYPE(tCmpxInt(8)) :: c(:)
            CHARACTER(*) :: iomsg
        END FUNCTION WriteCmpx8
    END INTERFACE WriteCmpx

    INTEGER, PARAMETER :: N = 5

    INTEGER(2) :: i
    INTEGER(4) :: j
    INTEGER(8) :: k

    INTEGER :: iostat

    CHARACTER(255) :: iomsg

    TYPE(tCmpxInt(2)) :: cmpxInt2( N )
    TYPE(tCmpxInt(4)) :: cmpxInt4( N )
    TYPE(tCmpxInt(8)) :: cmpxInt8( N )


    cmpxInt2 = [ (tCmpxInt(2)(i,(i + 1_2)), i = 1_2, (INT(N, 2) * 2_2), 2) ]
    cmpxInt4 = [ (tCmpxInt(4)(j,(j + 1_4)), j = 1_4, (INT(N, 4) * 2_4), 2) ]
    cmpxInt8 = [ (tCmpxInt(8)(k,(k + 1_8)), k = 1_8, (INT(N, 8) * 2_8), 2) ]


    OPEN(33, ACCESS='sequential', ACTION='write',&
         FORM='unformatted', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'OPEN(', iostat, '): ', iomsg( 1:LEN_TRIM( iomsg ) )
        STOP 10
    END IF


    iostat = WriteCmpx(33, cmpxInt2, iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'WRITE(', iostat, '): ', iomsg( 1:LEN_TRIM( iomsg ) )
        STOP 20
    END IF

    iostat = WriteCmpx(33, cmpxInt4, iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'WRITE(', iostat, '): ', iomsg( 1:LEN_TRIM( iomsg ) )
        STOP 30
    END IF

    iostat = WriteCmpx(33, cmpxInt8, iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'WRITE(', iostat, '): ', iomsg( 1:LEN_TRIM( iomsg ) )
        STOP 40
    END IF


    CLOSE(33, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'CLOSE(', iostat, '): ', iomsg( 1:LEN_TRIM( iomsg ) )
        STOP 50
    END IF

END PROGRAM sequentialWrite01


INTEGER FUNCTION WriteCmpx2(u, c, iomsg)
    USE tMod

    IMPLICIT NONE

    INTEGER :: u
    TYPE(tCmpxInt(2)) :: c(:)
    CHARACTER(*) :: iomsg


    WRITE(u, IOSTAT=WriteCmpx2, IOMSG=iomsg) c

END FUNCTION WriteCmpx2


INTEGER FUNCTION WriteCmpx4(u, c, iomsg)
    USE tMod

    IMPLICIT NONE

    INTEGER :: u
    TYPE(tCmpxInt(4)) :: c(:)
    CHARACTER(*) :: iomsg


    WRITE(u, IOSTAT=WriteCmpx4, IOMSG=iomsg) c

END FUNCTION WriteCmpx4


INTEGER FUNCTION WriteCmpx8(u, c, iomsg)
    USE tMod

    IMPLICIT NONE

    INTEGER :: u
    TYPE(tCmpxInt(8)) :: c(:)
    CHARACTER(*) :: iomsg


    WRITE(u, IOSTAT=WriteCmpx8, IOMSG=iomsg) c

END FUNCTION WriteCmpx8
