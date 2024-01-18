! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrAssumSiz.f
! opt variations: -ql

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrAssumSiz.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 09, 2006
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
!*  Assumed size array 
!*
!*  
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    INTEGER(K1)   :: ID
  CONTAINS
    PROCEDURE ModFun
  END TYPE

  CONTAINS

  ELEMENTAL FUNCTION ModFun(Arg)
  CLASS(DT(4)), INTENT(IN) :: Arg
  INTEGER   :: ModFun
    ModFun = Arg%ID
  END FUNCTION

  END MODULE


  PROGRAM dataPtrAssumSiz 
  USE M
  IMPLICIT NONE

  TYPE(DT(4)),     TARGET :: Arr(10,10)

  Arr = DT(4)(-1) 

  CALL S(Arr, 10, 0, 9)

  CONTAINS

  SUBROUTINE S(Arr, N, L, U)
  CLASS(*), TARGET  :: Arr(L:U, L:*)
  INTEGER           :: N, L, U
  CLASS(*), POINTER :: Ptr(:, :)


  Ptr(L:, L:) => Arr(:,L:U)

  IF (.NOT. ASSOCIATED(Ptr, Arr(:, L:U)))          STOP 11
  IF (ANY( LBOUND(Ptr)         .NE. (/L, L /)))    STOP 12
  IF (ANY( UBOUND(Ptr)         .NE. (/U, U /)))    STOP 13
  SELECT TYPE (Ptr)
  TYPE IS (DT(4))
    IF (ANY( Ptr%ID            .NE.   -1))         STOP 14
    IF (ANY( Ptr%ModFun()      .NE.   -1))         STOP 15
  CLASS DEFAULT
    STOP 16
  END SELECT

  Ptr(L:U, U:U) => Arr(:,L)

  IF (.NOT. ASSOCIATED(Ptr))                       STOP 21
  IF (ANY( LBOUND(Ptr)         .NE. (/L, U /)))    STOP 22
  IF (ANY( UBOUND(Ptr)         .NE. (/U, U /)))    STOP 23
  SELECT TYPE (Ptr)
  TYPE IS (DT(4))
    IF (ANY( Ptr%ID            .NE.   -1))         STOP 24
    IF (ANY( Ptr%ModFun()      .NE.   -1))         STOP 25
  CLASS DEFAULT
    STOP 26
  END SELECT


  END SUBROUTINE

  END



