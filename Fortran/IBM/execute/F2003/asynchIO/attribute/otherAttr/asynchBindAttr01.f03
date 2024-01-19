!*  ===================================================================
!*
!*                               Interactions with Other Attributes
!*
!*  DATE                       : February  8, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : Interactions with the BIND Attribute
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
!*                  or  language-binding-spec
!*
!*  5.1.2.4 BIND attribute for data entities
!*
!*  The BIND attribute for a variable or common block specifies that it is
!*  capable of interoperating with a C variable that has external linkage
!*  (15.3).
!*
!*  R509 language-binding-spec  is  BIND (C [, NAME =&
!*                                      &scalar-char-initialization-expr ])
!*
!*  5.2.4 BIND statement
!*
!*  R522 bind-stmt    is  language-binding-spec [ :: ] bind-entity-list
!*  R523 bind-entity  is  entity-name
!*                    or  / common-block-name /
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mShort
    IMPLICIT NONE

        INTEGER(KIND = 2), BIND(C, NAME='cShort') :: cShort

    INTERFACE
        INTEGER FUNCTION cshortdisp( )
        END FUNCTION cshortdisp
    END INTERFACE
END MODULE mShort


PROGRAM asynchBindAttr01
    USE mShort

    IMPLICIT NONE

    INTEGER :: iStatus
    INTEGER :: rStatus

    CHARACTER(LEN =256) :: errMsg


    ASYNCHRONOUS cShort


    OPEN(19, ASYNCHRONOUS='yes', ACTION='read',&
        &FILE="asynchBindAttr01.dat", IOMSG=errMsg,&
                &ACCESS='sequential', IOSTAT=iStatus)
    IF (iStatus <> 0) THEN
        WRITE(0, *) "OPEN(): <", iStatus, "> ", errMsg
        CALL zzrc( 1 )
    END IF


    READ(19, *, ASYNCHRONOUS='yes', IOSTAT=rStatus, IOMSG=errMsg) cShort
    IF (rStatus <> 0) THEN
        WRITE(0, *) "READ(): <", rStatus, "> ", errMsg
    END IF


    CLOSE(19, IOSTAT=iStatus, IOMSG=errMsg)
    IF (iStatus <> 0) THEN
        WRITE(0, *) "CLOSE(): <", iStatus, "> ", errMsg
        CALL zzrc( 3 )

    ELSE IF (rStatus <> 0) THEN
        CALL zzrc( 2 )
    END IF


    iStatus = cshortdisp( )
    IF (iStatus <> 0) THEN
        WRITE(0, *) "cshortdisp(): <", iStatus, ">"
        CALL zzrc( 4 )
    END IF


END PROGRAM asynchBindAttr01
