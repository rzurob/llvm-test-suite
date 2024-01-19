!*  ===================================================================
!*
!*                               Attribute in the ASSOCIATE Construct
!*
!*  DATE                       : February 24, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : associate-name => selector (where selector
!*                               explicitly has the ASYNCHRONOUS Attribute)
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

PROGRAM asynchAttrAssocName01

    IMPLICIT NONE


    INTEGER :: oStat
    INTEGER :: ioUnit = 4096

    INTEGER, DIMENSION( 1:39 ) :: aID

    CHARACTER(LEN = 256) :: oMsg

    INTEGER :: i
    INTEGER :: j

    CHARACTER(LEN = 256), PARAMETER, ASYNCHRONOUS :: helloWorld =&
                    "Hello, --YES-- --unformatted-- --stream-- World!"


    OPEN(UNIT=ioUnit, ASYNCHRONOUS=helloWorld( 10:12 ),&
                ACCESS=helloWorld( 34:39 ),IOSTAT=oStat,&
                        FORM=helloWorld( 18:28 ), IOMSG=oMsg)
    IF (oStat <> 0) THEN
        WRITE(0, *) "OPEN() <", oStat, "> ", oMsg
        CALL zzrc( 1 )
    END IF


    DO i = 1, 39
        j = i + 9

        ASSOCIATE (hello => helloWorld( i:j ))

            WRITE(ioUnit, ASYNCHRONOUS=helloWorld( 10:12 ), ID=aID( i ),&
                &IOSTAT=oStat, IOMSG=oMsg) hello, NEW_LINE( hello )
            IF (oStat <> 0) THEN
                WRITE(0, *) "WRITE() <", oStat, "> ", oMsg
                CALL zzrc( 2 )
            END IF

        END ASSOCIATE
    END DO


    DO i = 39, 1, -1
        WAIT(UNIT=ioUnit, ID=aID( i ), IOSTAT=oStat, IOMSG=oMsg)
        IF (oStat <> 0) THEN
            WRITE(0, *) i, ") WAIT(", aID( i ), ") <", oStat, "> ", oMsg
            CALL zzrc( 3 )
        END IF
    END DO


    CLOSE(UNIT=ioUnit, IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", oStat, "> ", oMsg
        CALL zzrc( 4 )
    END IF

END PROGRAM asynchAttrAssocName01
