! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=self -qreuse=base /tstdev/F2003/asynchIO/attribute/otherAttr/asynchTargetAttr01.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*                               Interactions with Other Attributes
!*
!*  DATE                       : February 16, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : Interactions with the TARGET Attribute
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
!*  R502 declaration-type-spec  is  intrinsic-type-spec
!*
!*  R503 attr-spec  is  access-spec
!*                  or  ALLOCATABLE
!*                  or  ASYNCHRONOUS
!*  ...
!*                  or  TARGET
!*
!*  5.1.2.14 TARGET attribute
!*
!*  An object with the TARGET attribute may have a pointer associated with
!*  it (7.4.2). An object without the TARGET attribute shall not have an
!*  accessible pointer associated with it.
!*
!*  5.2.13 TARGET statement
!*
!*  R546 target-stmt  is  TARGET [ :: ] object-name [ ( array-spec ) ]&
!*                               &[ , object-name [ ( array-spec ) ] ] ...
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mPoint
    IMPLICIT NONE

    TYPE tPoint(N1,K1)    ! (20,4)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
        INTEGER(K1)   :: x
        INTEGER(K1)   :: y
        INTEGER(K1)   :: z
    END TYPE tPoint
END MODULE mPoint


MODULE mPixel
    USE mPoint

    IMPLICIT NONE

    TYPE, EXTENDS(tPoint) :: tPixel    ! (20,4)
        INTEGER(K1) :: red
        INTEGER(K1) :: green
        INTEGER(K1) :: blue
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

    TYPE(tPixel(20,4)), TARGET :: pixel = tPixel(20,4)(29,33,56, 254,0,128)
    TYPE(tPoint(20,4)), TARGET, ASYNCHRONOUS :: ptr = tPoint(20,4)(99,42,86)

    CLASS(tPoint(:,4)), POINTER :: ptPtr


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
