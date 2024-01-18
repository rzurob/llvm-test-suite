!*  ===================================================================
!*
!*                               (CHARACTER Length > 1)
!*
!*  DATE                       : April 25, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE Attribute
!*  SECONDARY FUNCTIONS TESTED : Specified for non-Dummy Arguments
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 : VALUE Attribute
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 3
!*
!*  DESCRIPTION                :
!*
!*  5.1 Type declaration statements
!*
!*  R501 type-declaration-stmt  is  declaration-type-spec [ [ , attr-spec ]&
!*                                      &... :: ] entity-decl -list
!*
!*  R503 attr-spec              is  access-spec
!*                              or  ALLOCATABLE
!*                              or  ASYNCHRONOUS
!*  ...
!*                              or  VALUE
!*  ...
!*
!*  C515 (R501) The INTENT, VALUE, and OPTIONAL attributes may be specified
!*              only for dummy arguments.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM C515valueAttr001d

    INTERFACE
        SUBROUTINE gosub(arg1, arg2)
            CHARACTER(len = 5) :: arg1
            CHARACTER(len = 4), VALUE :: arg2
        END SUBROUTINE gosub
    END INTERFACE

    CHARACTER(len = 5) :: fiveLen = 'abcde'
    CHARACTER(len = 4), VALUE :: fourLen = 'ABCD'

    VALUE fiveLen


    CALL gosub(fiveLen, fourLen)
    IF (fourLen /= 'ABCD') THEN
        CALL zzrc( 1_4 )
    END IF

END PROGRAM C515valueAttr001d


SUBROUTINE gosub(arg1, arg2)
    CHARACTER(len = 5) :: arg1
    CHARACTER(len = 4), VALUE :: arg2


    CHARACTER(len = 2) :: sub2Len
    CHARACTER(len = 3), VALUE :: sub3Len

    VALUE sub30Len


    IF (arg1 /= 'abcde') THEN
        CALL zzrc( 2_4 )

    ELSE IF (arg2 /= 'ABCD') THEN
        CALL zzrc( 3_4 )
    END IF

    sub3Len = arg1( :3 )
    sub2Len = arg1( 4: )

    arg2 = sub3Len( 1:2 ) // sub2Len

END SUBROUTINE gosub
