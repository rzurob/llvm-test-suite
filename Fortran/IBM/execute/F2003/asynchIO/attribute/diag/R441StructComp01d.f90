!*  ===================================================================
!*
!*                               Attribute in Derived Types
!*
!*  DATE                       : January 16, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute in a Type Declaration
!*                               Statement
!*  SECONDARY FUNCTIONS TESTED : ASYNCHRONOUS Attribute in a Component Part
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  The ASYNCHRONOUS Attribute is not part of component-attr-spec:
!*
!*  4.5.1 Derived-type definition
!*
!*  R429 derived-type-def  is  derived-type-stmt
!*                                 [ type-param-def-stmt ] ...
!*                                 [ private-or-sequence ] ...
!*                                 [ component-part ]
!*                                 [ type-bound-procedure-part ]
!*                                 end-type-stmt
!*
!*  R430 derived-type-stmt  is  TYPE [ [ , type-attr-spec-list ] :: ]&
!*                                     &type-name [ ( type-param-name-list ) ]
!*
!*  R433 end-type-stmt  is  END TYPE [ type-name ]
!*
!*  R438 component-part  is  [ component-def-stmt ] ...
!*
!*  R439 component-def-stmt  is  data-component-def-stmt
!*                           or  proc-component-def-stmt
!*
!*  R440 data-component-def-stmt  is  declaration-type-spec&
!*                                      &[ [ , component-attr-spec-list ] :: ]&
!*                                      &component-decl -list
!*
!*  R441 component-attr-spec  is  POINTER
!*                            or  DIMENSION ( component-array-spec )
!*                            or  ALLOCATABLE
!*                            or  access-spec
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM R441StructComp01d

    TYPE tPoint
        REAL :: x, y
        CHARACTER(LEN = 10), ASYNCHRONOUS :: name
    END TYPE tPoint

    TYPE( tPoint ) :: asynchPoint

    asynchPoint%x = 5.2
    asynchPoint%y = 1.3
    asynchPoint%name = 'alpha'

END PROGRAM R441StructComp01d
