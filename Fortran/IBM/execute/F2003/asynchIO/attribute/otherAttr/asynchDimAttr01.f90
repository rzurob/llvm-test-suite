!*  ===================================================================
!*
!*                               Interactions with Other Attributes
!*
!*  DATE                       : February  8, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : Interactions with the DIMENSION Attribute
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  5.1 Type declaration statements
!*
!*  R501 type-declaration-stmt  is  declaration-type-spec [ [ , attr-spec ]&
!*                                      &... :: ] entity-decl-list
!*
!*  R502 declaration-type-spec is intrinsic-type-spec
!*
!*  R503 attr-spec  is  access-spec
!*                  or  ALLOCATABLE
!*                  or  ASYNCHRONOUS
!*                  or  DIMENSION ( array-spec )
!*
!*  5.1.2.5 DIMENSION attribute
!*
!*  R510 array-spec  is  explicit-shape-spec-list
!*                   or  assumed-shape-spec-list
!*                   or  deferred-shape-spec-list
!*                   or  assumed-size-spec
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM asynchDimAttr01
    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: j

    INTEGER :: iStat
    INTEGER :: rFail = 0
    INTEGER :: readID

    CHARACTER(LEN = 256) iMsg
    CHARACTER(LEN = 10), ASYNCHRONOUS, DIMENSION( 2,26 ) :: words


    OPEN(UNIT=42, FILE='asynchDimAttr01.dat',&
            &ASYNCHRONOUS='yes', ACTION='read',&
            &ACCESS='stream', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN(): <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    READ(UNIT=42, ASYNCHRONOUS='yes', ERR=100,&
        &ID=readID, IOSTAT=iStat, IOMSG=iMsg) (words( j,1 ), j = 1, 2)

    WAIT(UNIT=42, ID=readID, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT(): <", iStat, "> ", iMsg
        GOTO 200
    END IF

    DO i = 2, 26
        READ(UNIT=42, ASYNCHRONOUS='yes', ERR=100,&
            &ID=readID, IOSTAT=iStat, IOMSG=iMsg) (words( j,i ), j = 1, 2)

        CALL ProcessWords(words, (i - 1))

        WAIT(UNIT=42, ID=readID, IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat <> 0) THEN
            WRITE(0, *) "WAIT(): <", iStat, "> ", iMsg
            GOTO 200
        END IF
    END DO

    CALL ProcessWords(words, 26)
    GOTO 300


100 CONTINUE
    rFail = 2
    WRITE(0, *) "READ(): <", iStat, "> ", iMsg
    GOTO 300


200 CONTINUE
    rFail = 3


300 CONTINUE
    CLOSE(UNIT=42, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE(): <", iStat, "> ", iMsg
        CALL zzrc( 4 )


    ELSE IF (rFail <> 0) THEN
        CALL zzrc( rFail )
    END IF

END PROGRAM asynchDimAttr01


SUBROUTINE ProcessWords(wordList, idx)
    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: idx

    CHARACTER(LEN = 10), DIMENSION( 2,26 ) :: wordList


    PRINT 150, (wordList( i,idx ), i = 1, 2)
150 FORMAT(2('(',a10,')'))

END SUBROUTINE ProcessWords
