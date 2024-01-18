!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrArrSize.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 21, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289075 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  Array size = 1 
!*
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
    IF (SIZE(Ptr)  .NE. 1 )                  STOP 10
    IF (.NOT. ASSOCIATED(Ptr, Tar2))         STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/2, -2 /)))   STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/2, -2/)))    STOP 13
    IF (ANY( Ptr         .NE. -2))           STOP 14


    Ptr(-2:-2, 2:2) => Tar1 
    IF (SIZE(Ptr)  .NE. 1 )                  STOP 20
    IF (.NOT. ASSOCIATED(Ptr))               STOP 21
    IF (ANY( LBOUND(Ptr) .NE. (/-2, 2 /)))   STOP 22
    IF (ANY( UBOUND(Ptr) .NE. (/-2, 2/)))    STOP 23
    IF (ANY( Ptr         .NE. -1))           STOP 24


    Ptr(2:, -2:) => Tar22(::2, ::2) 
    IF (SIZE(Ptr)  .NE. 1 )                  STOP 30
    IF (.NOT. ASSOCIATED(Ptr, Tar22(::2, ::2)))  STOP 31
    IF (ANY( LBOUND(Ptr) .NE. (/2, -2 /)))   STOP 32
    IF (ANY( UBOUND(Ptr) .NE. (/2, -2/)))    STOP 33
    IF (ANY( Ptr         .NE. -2))           STOP 34


    Ptr(-2:-2, 2:2) => Tar11(::2) 
    IF (SIZE(Ptr)  .NE. 1 )                  STOP 40
    IF (.NOT. ASSOCIATED(Ptr))               STOP 41
    IF (ANY( LBOUND(Ptr) .NE. (/-2, 2 /)))   STOP 42
    IF (ANY( UBOUND(Ptr) .NE. (/-2, 2/)))    STOP 43
    IF (ANY( Ptr         .NE. -1))           STOP 44


  END



