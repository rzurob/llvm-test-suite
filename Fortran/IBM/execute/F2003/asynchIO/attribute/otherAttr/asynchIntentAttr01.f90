!*  ===================================================================
!*
!*                               Interactions with Other Attributes
!*
!*  DATE                       : February  8, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : Interactions with the INTENT Attribute
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
!*  ...
!*                  or INTENT ( intent-spec )
!*
!*  5.1.2.7 INTENT attribute
!*
!*  R517 intent-spec  is  IN
!*                    or  OUT
!*                    or  INOUT
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM asynchIntentAttr01
    IMPLICIT NONE

    INTEGER :: oStat
    INTEGER :: wFail = 0

    COMPLEX :: complexValue = (65.432 , 56.789)

    CHARACTER(LEN = 256) :: oMsg


    OPEN(57, ASYNCHRONOUS='yes', ACTION='write',&
        &ACCESS='stream', IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat /= 0) THEN
        WRITE(0, *) "OPEN(): <", oStat, "> ", oMsg
        CALL zzrc( 1 )
    END IF


    CALL IntentInOut( complexValue )


    WRITE(UNIT=57, ASYNCHRONOUS='yes', IOSTAT=ostat, IOMSG=oMsg) complexValue
    IF (oStat /= 0) THEN
        WRITE(0, *) "WRITE(): <", oStat, "> ", oMsg
        wFail = 2


    ELSE
        CALL IntentIn( complexValue )


        WAIT(UNIT=57, IOSTAT=ostat, IOMSG=oMsg)
        IF (oStat /= 0) THEN
            WRITE(0, *) "WAIT(): <", oStat, "> ", oMsg
            wFail = 3

        ELSE
            CALL IntentOut( complexValue )
        END IF
    END IF


    CLOSE(UNIT=57, IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat /= 0) THEN
        WRITE(0, *) "CLOSE(): <", oStat, "> ", oMsg
        CALL zzrc( 4 )

    ELSE IF (wFail /= 0) THEN
        CALL zzrc( wFail )
    END IF

END PROGRAM asynchIntentAttr01


SUBROUTINE IntentInOut( cValue )
    IMPLICIT NONE

    COMPLEX, INTENT(INOUT) :: cValue


    PRINT 100, cValue
100 FORMAT('IntentInOut() Complex Value = (',F6.3,',',F6.3,')')

    cValue = cValue + (1.0 , 1.0)

END SUBROUTINE IntentInOut


SUBROUTINE IntentIn( complexV )
    IMPLICIT NONE

    COMPLEX, INTENT(IN), ASYNCHRONOUS :: complexV


    PRINT 100, complexV
100 FORMAT('IntentIn()    Complex Value = (',F6.3,',',F6.3,')')

END SUBROUTINE IntentIn


SUBROUTINE IntentOut( cV )
    IMPLICIT NONE

    COMPLEX, INTENT(OUT) :: cV


    cV = cV - (1.0 , 1.0)

    PRINT 100, cV
100 FORMAT('IntentOut()   Complex Value = (',F6.3,',',F6.3,')')

END SUBROUTINE IntentOut
