! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 02, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The selector is a function return a section of allocatable array
!*    (ICE-305139)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM ArrFuncAllocSec1
  IMPLICIT CHARACTER(129)(C)
  INTEGER :: i


  ASSOCIATE ( As => Fun(RESHAPE((/C1, C2, C3, C4/), (/2,2/))) )

    IF ( ANY (LBOUND(As)  .NE. (/1,1/) ) )             ERROR STOP 30
    IF ( ANY (UBOUND(As)  .NE. (/2,2/) ) )             ERROR STOP 31
    IF ( ANY (SHAPE(As)   .NE. (/2,2/) ) )             ERROR STOP 32

    IF ( TRIM(As(1,1)) .NE. "1" ) ERROR STOP 33
    IF ( TRIM(As(2,1)) .NE. "2" ) ERROR STOP 34
    IF ( TRIM(As(1,2)) .NE. "3" ) ERROR STOP 35
    IF ( TRIM(As(2,2)) .NE. "4" ) ERROR STOP 35
    IF (  LEN(As)      .NE. 129 ) ERROR STOP 36

    ASSOCIATE ( As => FUN(As) )

      IF ( ANY (LBOUND(As)  .NE. (/1,1/) ) )             ERROR STOP 40
      IF ( ANY (UBOUND(As)  .NE. (/2,2/) ) )             ERROR STOP 41
      IF ( ANY (SHAPE(As)   .NE. (/2,2/) ) )             ERROR STOP 42

      IF ( TRIM(As(1,1)) .NE. "1" ) ERROR STOP 43
      IF ( TRIM(As(2,1)) .NE. "2" ) ERROR STOP 44
      IF ( TRIM(As(1,2)) .NE. "3" ) ERROR STOP 45
      IF ( TRIM(As(2,2)) .NE. "4" ) ERROR STOP 45
      IF ( LEN(As)       .NE. 129 ) ERROR STOP 46

    END ASSOCIATE
  END ASSOCIATE

  CONTAINS

  FUNCTION Fun(Arg)
  CHARACTER(*) :: Arg(:,:)
  CHARACTER(LEN(Arg)), ALLOCATABLE :: Fun(:,:)
    ALLOCATE(Fun(SIZE(Arg,1), SIZE(Arg,2)), SOURCE=Arg)
    Fun = RESHAPE((/"1","2","3","4"/), (/2,2/))
  END FUNCTION

  END

