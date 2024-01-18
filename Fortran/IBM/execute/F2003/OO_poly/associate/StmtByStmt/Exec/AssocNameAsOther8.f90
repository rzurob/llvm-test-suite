! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 01, 2005
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
!*    The associate construct name is the same as a structure name
!*   ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM AssocNameAsOther8
  INTEGER :: i, Arr(2:9)=-1

  GOTO 1
1 A1:  ASSOCIATE ( A1 => Arr)
    IF ( ANY(LBOUND(A1) .NE. (/2/) )) ERROR STOP 11
    IF ( ANY(SHAPE(A1)  .NE. (/8/) )) ERROR STOP 12
    IF ( ANY(A1    .NE. -1) )         ERROR STOP 13
    GO TO 2

2 A2: ASSOCIATE ( A2 => A1)
      IF ( ANY(LBOUND(A2) .NE. (/2/) )) ERROR STOP 11
      IF ( ANY(SHAPE(A2)  .NE. (/8/) )) ERROR STOP 12
      IF ( ANY(A2    .NE. -1) )         ERROR STOP 13

      A2 = 1
      IF ( ANY(Arr   .NE. 1) )         ERROR STOP 31
      IF ( ANY(A2    .NE. 1) )         ERROR STOP 32
      IF ( ANY(A1    .NE. 1) )         ERROR STOP 33
      GO TO 21

      ASSOCIATE ( A1 => A2)
        IF ( ANY(A1 .NE. Arr ))  ERROR STOP 44
      END ASSOCIATE

21    END ASSOCIATE A2
      GOTO 11

11  END ASSOCIATE A1
  IF ( ANY(Arr    .NE. 1) )         ERROR STOP 14

  END

