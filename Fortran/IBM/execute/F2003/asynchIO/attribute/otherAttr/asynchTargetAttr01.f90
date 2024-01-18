!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : asynchTargetAttr01 - ASYNCHRONOUS Attribute
!*                               Interactions with Other Attributes
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : February 16, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : Interactions with the TARGET Attribute
!*
!*  DRIVER STANZA              : xlf2003
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
!*  R502 declaration-type-spec  is  intrinsic-type-spec
!*
!*  R503 attr-spec  is  access-spec
!*                  or  ALLOCATABLE
!*                  or  ASYNCHRONOUS
!*  ...
!*                  or  TARGET
!*
!*
!*  5.1.2.14 TARGET attribute
!*
!*  An object with the TARGET attribute may have a pointer associated with
!*  it (7.4.2). An object without the TARGET attribute shall not have an
!*  accessible pointer associated with it.
!*
!*
!*  5.2.13 TARGET statement
!*
!*  R546 target-stmt  is  TARGET [ :: ] object-name [ ( array-spec ) ]&
!*                               &[ , object-name [ ( array-spec ) ] ] ...
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mPoint
    IMPLICIT NONE

    TYPE tPoint
        INTEGER :: x
        INTEGER :: y
        INTEGER :: z
    END TYPE tPoint
END MODULE mPoint


MODULE mPixel
    USE mPoint

    IMPLICIT NONE

    TYPE, EXTENDS(tPoint) :: tPixel
        INTEGER :: red
        INTEGER :: green
        INTEGER :: blue
    END TYPE tPixel
END MODULE mPixel


PROGRAM asynchTargetAttr01
    USE mPixel

    IMPLICIT NONE

    INTEGER :: wID
    INTEGER :: oStat
    INTEGER :: wStat
    INTEGER :: wtStat = 0

    CHARACTER(LEN = 256) :: oMsg

    TYPE(tPixel), TARGET :: pixel = tPixel(29,33,56, 254,0,128)
    TYPE(tPoint), TARGET, ASYNCHRONOUS :: ptr = tPoint(99,42,86)

    CLASS(tPoint), POINTER :: ptPtr


    OPEN(35, FILE='asynchTargetAttr01.dat',&
        &FORM='formatted', ASYNCHRONOUS='yes', IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat <> 0) THEN
        WRITE(0, *) "OPEN() <", oStat, "> ", oMsg
        CALL zzrc( 1 )
    END IF


    ptPtr => pixel


    WRITE(35, 200, ASYNCHRONOUS='no', IOSTAT=wStat, IOMSG=oMsg)&
            &'pixel', pixel%x, pixel%y, pixel%z

    IF (wStat == 0) THEN

        WRITE(35, 200, ASYNCHRONOUS='yes', ID=wID,&
                &IOSTAT=wStat, IOMSG=oMsg) ' ptr ', ptr%x, ptr%y, ptr%z

        IF (wStat == 0) THEN
            WRITE(35, 200, IOSTAT=wStat, IOMSG=oMsg)&
                    &'pixel', ptPtr%x, ptPtr%y, ptPtr%z
            IF (wStat <> 0) THEN
                WRITE(0, *) "WRITE() <", wStat, "> ", oMsg
                wStat = 4
            END IF

        ELSE
            WRITE(0, *) "WRITE(Asynchronous) <", wStat, "> ", oMsg
            wStat = 3
        END IF

    ELSE
        WRITE(0, *) "WRITE() <", wStat, "> ", oMsg
        wStat = 2
    END IF



    ptPtr => ptr


    IF (wStat == 0) THEN
        WRITE(35, 200, ASYNCHRONOUS='no', IOSTAT=wStat,&
                &IOMSG=oMsg) ' ptr ', ptPtr%x, ptPtr%y, ptPtr%z
        IF (wStat <> 0) THEN
            WRITE(0, *) "WRITE() <", wStat, "> ", oMsg
            wStat = 5
        END IF

        WAIT(UNIT=35, ID=wID, IOSTAT=wtStat, IOMSG=omsg)
        IF (wtStat <> 0) THEN
            WRITE(0, *) "WAIT() <", wtStat, "> ", oMsg
            wtStat = 6
        END IF
    END IF


    CLOSE(35, IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", oStat, "> ", oMsg
        CALL zzrc( 7 )

    ELSE IF (wtStat <> 0) THEN
        CALL zzrc( wtStat )

    ELSE IF (wStat <> 0) THEN
        CALL zzrc( wStat )
    END IF


200 FORMAT(a5,' = (',I2,',',I2,',',I2,')')

END PROGRAM asynchTargetAttr01
