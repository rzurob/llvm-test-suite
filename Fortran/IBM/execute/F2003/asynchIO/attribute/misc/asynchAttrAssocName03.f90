!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : asynchAttrAssocName03 - ASYNCHRONOUS
!*                               Attribute in the ASSOCIATE Construct
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : February 28, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : associate-name => selector (where selector
!*                               implicitly has the ASYNCHRONOUS Attribute)
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 : ASYNCHRONOUS Attribute, ASSOCIATE Construct
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  8.1.4.1 Form of the ASSOCIATE construct
!*
!*  R816 associate-construct  is  associate-stmt
!*                                    block
!*                                    end-associate-stmt
!*  R817 associate-stmt       is  [ associate-construct-name : ] ASSOCIATE&
!*                                 &( association-list )
!*  R818 association          is  associate-name => selector
!*  R819 selector             is  expr
!*                            or  variable
!*
!*
!*  8.1.4.3 Attributes of associate names
!*
!*  Within a SELECT TYPE or ASSOCIATE construct, ... The associating entity
!*  has the ASYNCHRONOUS, TARGET, or VOLATILE attribute if and only if the
!*  selector is a variable and has the attribute.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mSpecArg
    TYPE tSpecArg
        CHARACTER(LEN = 30) :: specVal
    END TYPE tSpecArg
END MODULE mSpecArg


MODULE mActionArg
    USE mSpecArg

    TYPE, EXTENDS(tSpecArg) :: tActionArg
        INTEGER :: specType
    END TYPE tActionArg
END MODULE mActionArg


MODULE mAsynchronousArg
    USE mSpecArg

    TYPE, EXTENDS(tSpecArg) :: tAsynchronousArg
        INTEGER :: specType
    END TYPE tAsynchronousArg
END MODULE mAsynchronousArg


MODULE mFileArg
    USE mSpecArg

    TYPE, EXTENDS(tSpecArg) :: tFileArg
        INTEGER :: specType
    END TYPE tFileArg
END MODULE mFileArg


PROGRAM asynchAttrAssocName03
    USE mActionArg
    USE mAsynchronousArg
    USE mFileArg

    IMPLICIT NONE

    INTERFACE
        SUBROUTINE WriteSpec(theSpecArg, theID)
            USE mSpecArg
            IMPLICIT NONE

            INTEGER, INTENT(OUT) :: theID
            CLASS(tSpecArg), ASYNCHRONOUS, INTENT(IN) :: theSpecArg
        END SUBROUTINE WriteSpec
    END INTERFACE


    INTEGER, DIMENSION( 3 ) :: aID

    INTEGER :: i

    INTEGER :: oStat
    CHARACTER(LEN = 256) :: oMsg

    TYPE(tActionArg) :: act = tActionArg('write',1)
    TYPE(tAsynchronousArg) :: asynch = tAsynchronousArg('yes',2)
    TYPE(tFileArg) :: file = tFileArg('asynchAttrAssocName03.dat',3)


    OPEN(2600, ACTION=act%specVal, FILE=file%specVal,&
        &ASYNCHRONOUS=asynch%specVal, IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat <> 0) THEN
        WRITE(0, *) "OPEN() <", oStat, "> ", oMsg
        CALL zzrc( 1 )
    END IF


    CALL WriteSpec(act, aID( 1 ))
    CALL WriteSpec(file, aID( 2 ))
    CALL WriteSpec(asynch, aID( 3 ))


    DO i = 1, 3
        WAIT(2600, ID=aID( i ), IOSTAT=oStat, IOMSG=oMsg)
        IF (oStat <> 0) THEN
            WRITE(0, *) "WAIT() <", oStat, "> ", oMsg
            CALL zzrc( 3 )
        END IF
    END DO


    CLOSE(2600, IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", oStat, "> ", oMsg
        CALL zzrc( 4 )
    END IF

END PROGRAM asynchAttrAssocName03


SUBROUTINE WriteSpec(theSpecArg, theID)
    USE mSpecArg

    IMPLICIT NONE

    INTEGER :: wStat
    CHARACTER(LEN = 256) :: wMsg

    INTEGER, INTENT(OUT) :: theID
    CLASS(tSpecArg), ASYNCHRONOUS, INTENT(IN) :: theSpecArg


    ASSOCIATE(thisSpec => theSpecArg)
        WRITE(2600, '(A30)', ASYNCHRONOUS='yes', ID=theID,&
                &IOSTAT=wStat, IOMSG=wMsg) thisSpec%specVal

        IF (wStat <> 0) THEN
            WRITE(0, *) "WRITE() <", wStat, "> ", wMsg
            CALL zzrc( 2 )
        END IF
    END ASSOCIATE

END SUBROUTINE WriteSpec
