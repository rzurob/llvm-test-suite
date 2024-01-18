!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDecIntrinsic1
!*
!*  DATE                       : Apr. 17, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  The basic syatax
!*  intrinsic-type-spec
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDecIntrinsic1

  INTEGER(KIND=1)  :: I1(1)       = -1
  INTEGER(KIND=2)  :: I2(I1%KIND) = I1%KIND
  INTEGER(KIND=4)  :: I4(I2%KIND) = I2%KIND
  INTEGER(KIND=8)  :: I8(I4%KIND) = I4%KIND

  LOGICAL(KIND=1)  :: L1(1)       = .TRUE.
  LOGICAL(KIND=2)  :: L2(I1%KIND) = L1%KIND .EQ. L1%KIND
  LOGICAL(KIND=4)  :: L4(I2%KIND) = L2%KIND .EQ. L2%KIND
  LOGICAL(KIND=8)  :: L8(I4%KIND) = L4%KIND .EQ. L4%KIND

  REAL(KIND=4)     :: R4(1)       = -1
  REAL(KIND=8)     :: R8(R4%KIND) = R4%KIND
  REAL(KIND=16)    :: R6(R8%KIND) = R8%KIND

  COMPLEX(KIND=4)  :: Z4(1)       = -1
  COMPLEX(KIND=8)  :: Z8(Z4%KIND) = Z4%KIND
  COMPLEX(KIND=16) :: Z6(Z8%KIND) = Z8%KIND



  IF ( I1%KIND            .NE.   1          ) STOP 11
  IF ( ANY( I1            .NE.   -1       ) ) STOP 12
  IF ( ANY( SHAPE( I1 )   .NE.   [1]      ) ) STOP 13

  IF ( I2%KIND            .NE.   2          ) STOP 14
  IF ( ANY( I2            .NE.   I1%KIND  ) ) STOP 15
  IF ( ANY( SHAPE( I2 )   .NE.   [I1%KIND]) ) STOP 16

  IF ( I4%KIND            .NE.   4          ) STOP 17
  IF ( ANY( I4            .NE.   I2%KIND  ) ) STOP 18
  IF ( ANY( SHAPE( I4 )   .NE.   [I2%KIND]) ) STOP 19

  IF ( I8%KIND            .NE.   8          ) STOP 21
  IF ( ANY( I8            .NE.   I4%KIND  ) ) STOP 22
  IF ( ANY( SHAPE( I8 )   .NE.   [I4%KIND]) ) STOP 23

  IF ( L1%KIND            .NE.   1          ) STOP 24
  IF ( ANY( L1            .NEQV. .TRUE.   ) ) STOP 25
  IF ( ANY( SHAPE( L1 )   .NE.   [1]      ) ) STOP 26

  IF ( L2%KIND            .NE.   2          ) STOP 27
  IF ( ANY( L2            .NEQV. .TRUE.   ) ) STOP 28
  IF ( ANY( SHAPE( L2 )   .NE.   [L1%KIND]) ) STOP 29

  IF ( L4%KIND            .NE.   4          ) STOP 31
  IF ( ANY( L4            .NEQV. .TRUE.   ) ) STOP 32
  IF ( ANY( SHAPE( L4 )   .NE.   [L2%KIND]) ) STOP 33

  IF ( L8%KIND            .NE.   8          ) STOP 34
  IF ( ANY( L8            .NEQV. .TRUE.   ) ) STOP 35
  IF ( ANY( SHAPE( L8 )   .NE.   [L4%KIND]) ) STOP 36

  IF ( R4%KIND            .NE.   4          ) STOP 37
  IF ( ANY( R4            .NE.   -1       ) ) STOP 38
  IF ( ANY( SHAPE( R4 )   .NE.   [1]      ) ) STOP 39

  IF ( R8%KIND            .NE.   8          ) STOP 37
  IF ( ANY( R8            .NE.   R4%KIND  ) ) STOP 38
  IF ( ANY( SHAPE( R8 )   .NE.   [R4%KIND]) ) STOP 39

  IF ( R6%KIND            .NE.   16         ) STOP 41
  IF ( ANY( R6            .NE.   R8%KIND  ) ) STOP 42
  IF ( ANY( SHAPE( R6 )   .NE.   [R8%KIND]) ) STOP 43

  IF ( Z4%KIND            .NE.   4          ) STOP 37
  IF ( ANY( Z4            .NE.   -1       ) ) STOP 38
  IF ( ANY( SHAPE( Z4 )   .NE.   [1]      ) ) STOP 39

  IF ( Z8%KIND            .NE.   8          ) STOP 41
  IF ( ANY( Z8            .NE.   Z4%KIND  ) ) STOP 42
  IF ( ANY( SHAPE( Z8 )   .NE.   [Z4%KIND]) ) STOP 43

  IF ( Z6%KIND            .NE.   16         ) STOP 44
  IF ( ANY( Z6            .NE.   Z8%KIND  ) ) STOP 45
  IF ( ANY( SHAPE( Z6 )   .NE.   [Z8%KIND]) ) STOP 46



  END

