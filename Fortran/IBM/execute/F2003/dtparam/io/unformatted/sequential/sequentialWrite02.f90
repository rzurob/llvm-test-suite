!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : October  2, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Extended Derived Type (with Type Parameters)
!*                               Output to a SEQUENTIAL File
!*  SECONDARY FUNCTIONS TESTED : Output is performed by External Procedures
!*                               referenced via an INTERFACE (Procedure
!*                               selected via both Derived Type and KIND
!*                               Parameter values)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To Write an array of Derived Type through an External FUNCTION defined
!*  via an INTERFACE (based on Derived Type and KIND).
!*
!23456789012345678901234567890123456789012345678901234567890123456789012


PROGRAM sequentialWrite02
    USE tEMod1
    USE tEMod2

    IMPLICIT NONE

    INTERFACE WriteCmpx
        INTEGER FUNCTION WriteCmpx2(u, c, iomsg)
            USE tEMod1
            INTEGER :: u
            TYPE(tCmpxIntLable1(2,*)) :: c(:)
            CHARACTER(*) :: iomsg
        END FUNCTION WriteCmpx2

        INTEGER FUNCTION WriteCmpx4(u, c, iomsg)
            USE tEMod2
            INTEGER :: u
            TYPE(tCmpxIntLable2(4,*)) :: c(:)
            CHARACTER(*) :: iomsg
        END FUNCTION WriteCmpx4

        INTEGER FUNCTION WriteCmpx8(u, c, iomsg)
            USE tEMod1
            INTEGER :: u
            TYPE(tCmpxIntLable1(8,*)) :: c(:)
            CHARACTER(*) :: iomsg
        END FUNCTION WriteCmpx8
    END INTERFACE WriteCmpx

    INTEGER, PARAMETER :: N = 5

    INTEGER(2) :: i
    INTEGER(4) :: j
    INTEGER(8) :: k

    INTEGER :: iostat

    CHARACTER(255) :: iomsg

    TYPE(tCmpxIntLable1(2,8))  :: cmpxInt2( N )
    TYPE(tCmpxIntLable2(4,12)) :: cmpxInt4( N )
    TYPE(tCmpxIntLable1(8,5))  :: cmpxInt8( N )


    cmpxInt2 =&
        [ (tCmpxIntLable1(2,8)(i,(i + 1_2),'CmpxInt2'),&
                            i = 1_2, (INT(N, 2) * 2_2), 2) ]
    cmpxInt4 =&
        [ (tCmpxIntLable2(4,12)(j,(j + 1_4),'CMPLX_INT(4)'),&
                            j = 1_4, (INT(N, 4) * 2_4), 2) ]
    cmpxInt8 =&
        [ (tCmpxIntLable1(8,5)(k,(k + 1_8),'CI(8)'),&
                            k = 1_8, (INT(N, 8) * 2_8), 2) ]


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

END PROGRAM sequentialWrite02


INTEGER FUNCTION WriteCmpx2(u, c, iomsg)
    USE tEMod1

    IMPLICIT NONE

    INTEGER :: u
    TYPE(tCmpxIntLable1(2,*)) :: c(:)
    CHARACTER(*) :: iomsg


    WRITE(u, IOSTAT=WriteCmpx2, IOMSG=iomsg) c

END FUNCTION WriteCmpx2


INTEGER FUNCTION WriteCmpx4(u, c, iomsg)
    USE tEMod2

    IMPLICIT NONE

    INTEGER :: u
    TYPE(tCmpxIntLable2(4,*)) :: c(:)
    CHARACTER(*) :: iomsg


    WRITE(u, IOSTAT=WriteCmpx4, IOMSG=iomsg) c

END FUNCTION WriteCmpx4


INTEGER FUNCTION WriteCmpx8(u, c, iomsg)
    USE tEMod1

    IMPLICIT NONE

    INTEGER :: u
    TYPE(tCmpxIntLable1(8,*)) :: c(:)
    CHARACTER(*) :: iomsg


    WRITE(u, IOSTAT=WriteCmpx8, IOMSG=iomsg) c

END FUNCTION WriteCmpx8
