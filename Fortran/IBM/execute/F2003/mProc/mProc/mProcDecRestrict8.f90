!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 14, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Generaliztion of PROCEDURE statement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 296676
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  unAmbiguity
!*  (3) at least one of them shall have both
!*  (a) A nonoptional non-passed-object dummy argument at an effective position such that
!*      either the other procedure has no dummy argument at that effective position or the
!*      dummy argument at that position is distinguishable with it; and
!*  (b) A nonoptional non-passed-object dummy argument whose name is such that either
!*      the other procedure has no dummy argument with that name or the dummy argument
!*      with that name is distinguishable with it.
!*      Further, the dummy argument that disambiguates by position shall either be the same as
!*      or occur earlier in the argument list than the one that disambiguates by name.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  CONTAINS

  FUNCTION S3A(W,X,Y,Z)
  REAL :: W,Y
  INTEGER :: X,Z, S3A
    S3A = 1
  END FUNCTION

  FUNCTION S3B(X,W,Z,Y)
  REAL :: W,Z
  INTEGER :: X,Y, S3B
    S3B = 2
  ENDFUNCTION

  FUNCTION F2A(X)
  REAL :: F2A,X
   F2A = X
  END FUNCTION

  FUNCTION F2B(X,Y)
  COMPLEX :: F2B
  REAL :: X,Y
    F2B = (X,Y)
  END FUNCTION

  END MODULE

  PROGRAM mProcDecRestrict8
  USE M

  INTERFACE GFace1
    PROCEDURE F2A
    PROCEDURE F2B
  END INTERFACE

  IF (GFace1(1.0)  .NE. 1.0   )               STOP 21
  IF (GFace1(0.0, Y=1.0)  .NE. (0.0, 1.0)   ) STOP 22


  CALL IntSub(S3A)

  CONTAINS

  SUBROUTINE IntSub(Proc)
  PROCEDURE(S3A)           :: Proc
  PROCEDURE(S3B), POINTER  :: ProcPtr

  INTERFACE GFace
    PROCEDURE Proc
    PROCEDURE ProcPtr
  END INTERFACE

  ProcPtr => S3B

  IF (GFace(1.0, 2, 3.0, 4)  .NE. 1   )        STOP 11
  IF (GFace(X=2, W=1.0, Z=4., y=3)  .NE. 2   ) STOP 12

  END SUBROUTINE

  END



