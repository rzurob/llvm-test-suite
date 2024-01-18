!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 21, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289075
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Array size = 1
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrArrSize
  IMPLICIT NONE


  INTEGER,  TARGET  :: Tar2(-1:-1, -1:-1), Tar22(2,2)
  INTEGER,  TARGET ::  Tar1(-1:-1), Tar11(2)
  INTEGER,  POINTER :: Ptr(:, :)
  INTEGER    :: I, J, K, N

  N = 1; K = 0

  Tar2 = RESHAPE((/-2/), (/N, N/))
  Tar1 = (/-1/)

  Tar22 = RESHAPE((/-2/), (/N, N/))
  Tar11 = -1

    Ptr(2:, -2:) => Tar2
    IF (SIZE(Ptr)  .NE. 1 )                  ERROR STOP 10
    IF (.NOT. ASSOCIATED(Ptr, Tar2))         ERROR STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/2, -2 /)))   ERROR STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/2, -2/)))    ERROR STOP 13
    IF (ANY( Ptr         .NE. -2))           ERROR STOP 14


    Ptr(-2:-2, 2:2) => Tar1
    IF (SIZE(Ptr)  .NE. 1 )                  ERROR STOP 20
    IF (.NOT. ASSOCIATED(Ptr))               ERROR STOP 21
    IF (ANY( LBOUND(Ptr) .NE. (/-2, 2 /)))   ERROR STOP 22
    IF (ANY( UBOUND(Ptr) .NE. (/-2, 2/)))    ERROR STOP 23
    IF (ANY( Ptr         .NE. -1))           ERROR STOP 24


    Ptr(2:, -2:) => Tar22(::2, ::2)
    IF (SIZE(Ptr)  .NE. 1 )                  ERROR STOP 30
    IF (.NOT. ASSOCIATED(Ptr, Tar22(::2, ::2)))  ERROR STOP 31
    IF (ANY( LBOUND(Ptr) .NE. (/2, -2 /)))   ERROR STOP 32
    IF (ANY( UBOUND(Ptr) .NE. (/2, -2/)))    ERROR STOP 33
    IF (ANY( Ptr         .NE. -2))           ERROR STOP 34


    Ptr(-2:-2, 2:2) => Tar11(::2)
    IF (SIZE(Ptr)  .NE. 1 )                  ERROR STOP 40
    IF (.NOT. ASSOCIATED(Ptr))               ERROR STOP 41
    IF (ANY( LBOUND(Ptr) .NE. (/-2, 2 /)))   ERROR STOP 42
    IF (ANY( UBOUND(Ptr) .NE. (/-2, 2/)))    ERROR STOP 43
    IF (ANY( Ptr         .NE. -1))           ERROR STOP 44


  END



