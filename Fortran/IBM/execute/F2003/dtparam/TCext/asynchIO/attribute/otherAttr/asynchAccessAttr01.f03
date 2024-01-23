! GB DTP extension using:
! ftcx_dtp -ql -qreuse=self /tstdev/F2003/asynchIO/attribute/otherAttr/asynchAccessAttr01.f
! opt variations: -qnol -qreuse=none

!*  ===================================================================
!*
!*                               Interactions with Other Attributes
!*
!*  DATE                       : February  8, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : Interactions with Accessibility Attribute(s)
!*                               PUBLIC/PRIVATE
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
!*
!*  5.1.2.1 Accessibility attribute
!*
!*  The accessibility attribute specifies the accessibility of an entity
!*  via a particular identifier.
!*
!*  R508 access-spec  is  PUBLIC
!*                    or  PRIVATE
!*
!*  C539 (R508) An access-spec shall appear only in the specification-part
!*              of a module.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mPublic
    PUBLIC

        TYPE tPublicPt(N1,K1)    ! (20,4)
            INTEGER, KIND :: K1
            INTEGER, LEN  :: N1
            REAL(K1)      :: x
            REAL(K1)      :: y
        END TYPE tPublicPt

        TYPE(tPublicPt(20,4)), PRIVATE, ASYNCHRONOUS :: publicPrivatePt
        TYPE(tPublicPt(20,4)), ASYNCHRONOUS :: publicPublicPt
        TYPE(tPublicPt(20,4)) :: publicDefaultPt

    CONTAINS

        SUBROUTINE SetPrivatePt(anX, anY)
            IMPLICIT NONE

            REAL :: anX
            REAL :: anY


            publicPrivatePt = tPublicPt(20,4)(anX , anY)

        END SUBROUTINE SetPrivatePt


        INTEGER FUNCTION WritePrivatePt(unitID, recNum)
            IMPLICIT NONE

            INTEGER :: oStat
            INTEGER :: unitID
            INTEGER :: recNum

            CHARACTER(LEN = 256) :: oMsg


            WRITE(unitID, FMT=100, IOSTAT=oStat, REC=recNum,&
                    &IOMSG=oMsg, ASYNCHRONOUS='no') publicPrivatePt
            IF (oStat <> 0) THEN
                WRITE(0, *) "WRITE(): <", oStat, "> ", oMsg
            END IF

100         FORMAT('Pt = (',F7.3,',',F7.3,')')

            WritePrivatePt = oStat

        END FUNCTION WritePrivatePt
END MODULE mPublic


PROGRAM asynchAccessAttr01
    USE mPublic

    IMPLICIT NONE


    INTEGER :: wStat
    INTEGER :: ioStatus
    CHARACTER(LEN = 256) :: errMsg


    OPEN(UNIT=27, FILE='asynchAccessAttr01.dat',&
            &RECL=22, ACCESS='direct', IOMSG=errMsg,&
            &FORM='formatted', ASYNCHRONOUS='yes', IOSTAT=ioStatus)
    IF (ioStatus <> 0) THEN
        WRITE(0, *) "OPEN(): <", ioStatus, "> ", errMsg
        ERROR STOP 1

    ELSE
        publicPublicPt = tPublicPt(20,4)( 234.123,808.5 )
        publicDefaultPt = tPublicPt(20,4)( 23.6,65.2 )

        CALL SetPrivatePt(publicPublicPt%x, publicDefaultPt%y)


        WRITE(27, FMT=100, IOSTAT=wStat, REC=3,&
                &IOMSG=errMsg, ASYNCHRONOUS='yes') publicPublicPt
        IF (wStat <> 0) THEN
            WRITE(0, *) "WRITE(Asynchronous): <", wStat, "> ", errMsg
        END IF


        IF (wStat == 0) THEN
            wStat = WritePrivatePt(27, 1)
        END IF


        IF (wStat == 0) THEN
            WRITE(27, FMT=100, IOSTAT=wStat, IOMSG=errMsg,&
                    &REC=2, ASYNCHRONOUS='yes') publicDefaultPt
            IF (wStat <> 0) THEN
                WRITE(0, *) "WRITE(Asynchronous): <", wStat, "> ", errMsg
            END IF
        END IF


        CLOSE(IOSTAT=ioStatus, UNIT=27, IOMSG=errMsg)
        IF (ioStatus <> 0) THEN
            WRITE(0, *) "CLOSE(): <", ioStatus, "> ", errMsg
            ERROR STOP 3

        ELSE IF (wStat <> 0) THEN
            ERROR STOP 2
        END IF
    END IF


100 FORMAT('Pt = (',F7.3,',',F7.3,')')

END PROGRAM asynchAccessAttr01
