!*  ===================================================================
!*
!*                               Interactions with Other Attributes
!*
!*  DATE                       : February 17, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : Interactions with the VOLATILE Attribute
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
!*  R503 attr-spec      is  access-spec
!*                      or  ALLOCATABLE
!*                      or  ASYNCHRONOUS
!*  ...
!*                      or  VOLATILE
!*
!*  5.1.2.16 VOLATILE attribute
!*
!*  The VOLATILE attribute specifies that an object may be referenced,
!*  defined, or become undefined, by means not specified by the program.
!*
!*  5.2.15 VOLATILE statement
!*
!*  R548 volatile-stmt  is  VOLATILE [ :: ] object-name-list
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mAtype
    IMPLICIT NONE

    TYPE tAtype
	    INTEGER :: aComponent
	    REAL :: bComponent
    END TYPE tAtype
END MODULE mAtype


PROGRAM asynchVolatileAttr01
    USE mAtype

    IMPLICIT NONE

    INTEGER :: stat
    INTEGER :: record = 5

    CHARACTER(LEN = 256) :: msg

    TYPE(tAtype), VOLATILE, ASYNCHRONOUS :: aType


    OPEN(56, ASYNCHRONOUS='yes', ACCESS='direct',&
        &FORM='formatted', RECL=10, IOSTAT=stat, IOMSG=msg)
    IF (stat /= 0) THEN
        WRITE(*, *) "OPEN() <", stat, "> ", msg
        CALL zzrc( 1 )
    END IF


    aType = tAtype(23,65.1)
    WRITE(56, 9999, ASYNCHRONOUS='yes',&
            &REC=record, IOSTAT=stat, IOMSG=msg) aType
    IF (stat /= 0) THEN
        WRITE(*, *) "WRITE(Asynchronous,", record, ") <", stat, "> ", msg
        CALL zzrc( 2 )
    END IF


    aType = tAtype(635,13.561)
    WRITE(56, 9999, ASYNCHRONOUS='no',&
            &REC=record, IOSTAT=stat, IOMSG=msg) aType
    IF (stat /= 0) THEN
        WRITE(*, *) "WRITE(", record, ") <", stat, "> ", msg
        CALL zzrc( 3 )
    END IF


    CLOSE(56, IOSTAT=stat, IOMSG=msg)
    IF (stat /= 0) THEN
        WRITE(*, *) "CLOSE() <", stat, "> ", msg
        CALL zzrc( 4 )
    END IF


9999 FORMAT(I3,F6.3)

END PROGRAM asynchVolatileAttr01