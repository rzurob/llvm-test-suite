!*  ===================================================================
!*
!*                               Attribute in the ASSOCIATE Construct
!*
!*  DATE                       : February 28, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : associate-name => selector (where selector
!*                               implicitly has the ASYNCHRONOUS Attribute)
!*
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
!*  8.1.4.3 Attributes of associate names
!*
!*  Within a SELECT TYPE or ASSOCIATE construct, ... The associating entity
!*  has the ASYNCHRONOUS, TARGET, or VOLATILE attribute if and only if the
!*  selector is a variable and has the attribute.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mType
    TYPE tType
        INTEGER :: iType
    END TYPE tType
END MODULE mType

PROGRAM asynchAttrAssocName04
    USE mType

    IMPLICIT NONE

    INTEGER, PARAMETER :: n = 1000

    INTEGER :: i
    INTEGER :: j
    INTEGER :: oStat

    INTEGER :: idA( n )

    CHARACTER(LEN = 256) :: oMsg

    TYPE(tType), DIMENSION( n ) :: thatType
    TYPE(tType), DIMENSION( n ) :: thisType = (/ (tType(i), i = 1, n) /)


    OPEN(2006, ACCESS='direct', ACTION='readwrite', RECL=5,&
        &FORM='formatted', ASYNCHRONOUS='yes', IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat <> 0) THEN
        WRITE(0, *) "OPEN() <", oStat, "> ", oMsg
        CALL zzrc( 1 )
    END IF


    DO i = 1, n
        WRITE(2006, FMT='(I5)', ASYNCHRONOUS='yes', REC=i, ID=idA( i ),&
                &IOSTAT=oStat, IOMSG=oMsg) thisType( ((n - i) + 1) )%iType
        IF (oStat <> 0) THEN
            WRITE(0, *) i, ") WRITE() <", oStat, "> ", oMsg
            CALL zzrc( 2 )
        END IF
    END DO


    DO i = n, 1, -1
        j = (n - i) + 1

        WAIT(2006, ID=idA( j ), IOSTAT=oStat, IOMSG=oMsg)
        IF (oStat <> 0) THEN
            WRITE(0, *) j, ") WAIT() <", oStat, "> ", oMsg
            CALL zzrc( 3 )
        END IF


        ASSOCIATE(otherType => thatType( j ), verType => thisType( j ))
            READ(2006, FMT='(I5)', ASYNCHRONOUS='no', REC=i,&
                    &IOSTAT=oStat, IOMSG=oMsg) otherType%iType
            IF (oStat <> 0) THEN
                WRITE(0, *) j, ") READ() <", oStat, "> ", oMsg
                CALL zzrc( 4 )
            END IF

            IF (otherType%iType <> verType%iType) THEN
                WRITE(0, *) j, ") otherType%iType = '", otherType%iType,&
                            &"', verType%iType = '", verType%iType, "'"
                CALL zzrc( 5 )
            END IF
        END ASSOCIATE
    END DO


    CLOSE(2006, IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", oStat, "> ", oMsg
        CALL zzrc( 6 )
    END IF

END PROGRAM asynchAttrAssocName04
