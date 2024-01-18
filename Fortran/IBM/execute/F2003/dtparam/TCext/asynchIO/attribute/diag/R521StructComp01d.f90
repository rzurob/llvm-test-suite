! GB DTP extension using:
! ftcx_dtp -qck /tstdev/F2003/asynchIO/attribute/diag/R521StructComp01d.f
! opt variations: -qnock

!*  ===================================================================
!*
!*                               Attribute in Derived Types
!*
!*  DATE                       : January 17, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : R521
!*  SECONDARY FUNCTIONS TESTED : Derived Type -- Structure Component
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  A Structure Component cannot be specified as having the ASYNCHRONOUS
!*  Attribute using the ASYNCHRONOUS Statement:
!*
!*  5.2.3  ASYNCHRONOUS Statement
!*
!*  R521 asynchronous-stmt is ASYNCHRONOUS [ :: ] object-name-list
!*
!*  5.1  Type Declaration Statements
!*
!*  R505 object-name is name
!*
!*  3.2.1  Names
!*
!*  R304 name is letter [ alphanumeric-character ] ...
!*
!*  3.1  Processor Character Set
!*
!*  R302 alphanumeric-character is letter
!*                              or digit
!*                              or underscore
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM R521StructComp01d

    TYPE tPoint(K1,K2,N1)    ! (4,1,10)
        INTEGER, KIND             :: K1,K2
        INTEGER, LEN              :: N1
        REAL(K1)                  :: x, y
        CHARACTER(kind=K2,len=N1) :: name
    END TYPE tPoint

    TYPE( tPoint(4,1,10) ) :: asynchPoint
    ASYNCHRONOUS :: asynchPoint%x

    asynchPoint%x = 5.2
    asynchPoint%y = 1.3
    asynchPoint%name = 'alpha'

END PROGRAM R521StructComp01d
