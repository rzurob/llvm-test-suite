!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrPassObj.f  
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
!*  The passed object 
!*
!*  
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT
    INTEGER(1),  POINTER :: PtrI1(:, :)
    INTEGER(1),  POINTER :: PtrI2(:, :)
    INTEGER(1)           :: I1Tar(10,10)
  CONTAINS
    PROCEDURE, PASS :: Fun => ModFun  
  END TYPE

  TYPE (DT), SAVE, TARGET   :: T

  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(DT),  TARGET :: Arg
  CLASS(DT), POINTER :: ModFun
    Arg%PtrI1(0:, 0:) => Arg%I1Tar
    Arg%PtrI2(0:9, 0:0) => Arg%I1Tar(:, 1)
    ModFun => Arg
  END FUNCTION 

  END MODULE
 
  PROGRAM dataPtrPassObj 
  USE M
  IMPLICIT NONE
  TYPE (DT), SAVE, TARGET   :: V

  V%I1Tar = 1
  T = V%Fun()
 
  IF (.NOT. ASSOCIATED(T%PtrI1, V%I1Tar))              STOP 11
  IF (ANY( LBOUND(T%PtrI1)         .NE. (/0, 0 /)))    STOP 12
  IF (ANY( UBOUND(T%PtrI1)         .NE. (/9, 9 /)))    STOP 13
  IF (ANY( T%PtrI1                 .NE. 1_1))          STOP 14

  IF (.NOT. ASSOCIATED(T%PtrI2))                       STOP 15
  IF (ANY( LBOUND(T%PtrI2)         .NE. (/0, 0 /)))    STOP 16
  IF (ANY( UBOUND(T%PtrI2)         .NE. (/9, 0 /)))    STOP 17
  IF (ANY( T%PtrI2                 .NE. 1_1))          STOP 18

  END


