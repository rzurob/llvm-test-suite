!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrAssumShp1.f  
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
!*  Assumed shape array 
!*
!*  zero size 
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrAssumShp1 
  IMPLICIT NONE

  TYPE :: DT
    INTEGER :: ID
  END TYPE


  TYPE (DT),  TARGET :: Arr1(10, 10)
  TYPE (DT),  TARGET :: Arr2(1:0)

  INTEGER           :: N, L, U

  N =10
  L = 0
  U = 9

  CALL S(Arr1(:,1:0), Arr2, 0, 9)

  CONTAINS

  SUBROUTINE S(Arr1, Arr2, L, U)
  CLASS(DT), TARGET  :: Arr1(L:, L:)
  TYPE(DT),  TARGET  :: Arr2(L:)
  INTEGER           :: L, U
  CLASS(*), POINTER :: Ptr(:, :)

  Ptr(L:, L:) => Arr1
  IF ( .NOT. ASSOCIATED(Ptr))                       STOP 11
  IF (ANY( LBOUND(Ptr)         .NE. (/L, 1 /)))     STOP 12
  IF (ANY( SHAPE (Ptr)         .NE. (/U-L+1, 0 /))) STOP 13

  Ptr(L:U, L:L-1) => Arr2
  IF ( .NOT. ASSOCIATED(Ptr))                       STOP 21
  IF (ANY( LBOUND(Ptr)         .NE. (/L, 1 /)))     STOP 22
  IF (ANY( SHAPE (Ptr)         .NE. (/U-L+1, 0 /))) STOP 23

  END SUBROUTINE

  END


