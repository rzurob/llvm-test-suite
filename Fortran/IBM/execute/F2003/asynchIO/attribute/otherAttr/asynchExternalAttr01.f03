!*  ===================================================================
!*
!*                               Interactions with Other Attributes
!*
!*  DATE                       : February  9, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : Interactions with the EXTERNAL Attribute
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
!*                  or  EXTERNAL
!*
!*  5.1.2.6 EXTERNAL attribute
!*
!*  The EXTERNAL attribute specifies that an entity is an external procedure,
!*  dummy procedure, procedure pointer, or block data subprogram. This
!*  attribute may also be specified by an EXTERNAL statement (12.3.2.2), a
!*  procedure-declaration-stmt (12.3.2.3) or an interface body that is not
!*  in an abstract interface block (12.3.2.1).
!*
!*  12.3.2.2 EXTERNAL statement
!*
!*  An EXTERNAL statement specifies the EXTERNAL attribute (5.1.2.6) for a
!*  list of names.
!*
!*  R1210 external-stmt  is  EXTERNAL [ :: ] external-name-list
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM asynchExternalAttr01
    USE mPoint

    IMPLICIT NONE

    INTEGER :: oStat
    INTEGER :: wStat
    CHARACTER(LEN = 256) :: oMsg

    TYPE(tPoint) :: thePt

    COMMON /externalCommon/ thePt


    OPEN(UNIT=16, FILE='asynchExternalAttr01.dat',&
        &ACTION='write', ACCESS='stream', IOMSG=oMsg,&
        &ASYNCHRONOUS='yes', FORM='formatted', IOSTAT=oStat)
    IF (oStat <> 0) THEN
        WRITE(0, *) "OPEN():  <", oStat, "> ", oMsg
        ERROR STOP 1
    END IF


10  FORMAT('thePt = (',F6.3,',',F6.3,')')
    WRITE(UNIT=16, FMT=10, ASYNCHRONOUS='yes', IOSTAT=wStat, IOMSG=oMsg) thePt
    IF (wStat <> 0) THEN
        WRITE(0, *) "WRITE():  <", wStat, "> ", oMsg
    END IF


    CLOSE(UNIT=16, IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat <> 0) THEN
        WRITE(0, *) "CLOSE():  <", oStat, "> ", oMsg
        ERROR STOP 3

    ELSE IF (wStat <> 0) THEN
        ERROR STOP 2
    END IF

END PROGRAM asynchExternalAttr01
