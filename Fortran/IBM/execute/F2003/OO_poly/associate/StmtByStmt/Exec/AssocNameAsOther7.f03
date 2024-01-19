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
!*    The associate construct name is the same as a common blk name
!*   ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM AssocNameAsOther7
  INTEGER :: i, Arr(2:9)
  COMMON /cbk/i, Arr

  ASSOCIATE ( Cbk => Arr)
    IF ( ANY(LBOUND(cbk) .NE. (/2/) )) ERROR STOP 11
    IF ( ANY(SHAPE(cbk)  .NE. (/8/) )) ERROR STOP 12
    IF ( ANY(cbk    .NE. -1) )         ERROR STOP 13

    ASSOCIATE ( Init => Cbk)
      IF ( ANY(LBOUND(Init) .NE. (/2/) )) ERROR STOP 11
      IF ( ANY(SHAPE(Init)  .NE. (/8/) )) ERROR STOP 12
      IF ( ANY(Init    .NE. -1) )         ERROR STOP 13

      Init = 1
      IF ( ANY(Arr    .NE. 1) )         ERROR STOP 31
      IF ( ANY(Init   .NE. 1) )         ERROR STOP 32
      IF ( ANY(Cbk    .NE. 1) )         ERROR STOP 33

    END ASSOCIATE
  END ASSOCIATE
  IF ( ANY(Arr    .NE. 1) )         ERROR STOP 14

  END

  BLOCK DATA INIT

  INTEGER :: i, Arr(2:9)
  COMMON /cbk/i, Arr
  DATA Arr /8*-1/
  END BLOCK DATA
