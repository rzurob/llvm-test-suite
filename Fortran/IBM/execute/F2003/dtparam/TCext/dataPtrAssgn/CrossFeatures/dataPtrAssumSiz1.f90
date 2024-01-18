! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrAssumSiz1.f
! opt variations: -qnol -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrAssumSiz1.f  
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
!*  If the actual argument associated with the assumed-size dummy array is an array of any
!*  type other than default character, the size of the dummy array is that of the actual array. 
!*  
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: ID
  CONTAINS
    PROCEDURE ModFun
  END TYPE

  CONTAINS

  ELEMENTAL FUNCTION ModFun(Arg)
  CLASS(DT(*,4)), INTENT(IN) :: Arg
  INTEGER   :: ModFun
    ModFun = Arg%ID
  END FUNCTION

  END MODULE


  PROGRAM dataPtrAssumSiz1 
  USE M
  IMPLICIT NONE

  CLASS(DT(:,4)), POINTER :: Ptr(:,:)
  INTEGER :: L, U, N

  L = 0
  U = 9
  N = 10
  
  ALLOCATE( Ptr(10, 10), SOURCE=DT(20,4)(-1))

  CALL S(Ptr, Ptr, N, L, U)

  IF (.NOT. ASSOCIATED(Ptr))                       STOP 11
  IF (ANY( LBOUND(Ptr)         .NE. (/L, L /)))    STOP 12
  IF (ANY( UBOUND(Ptr)         .NE. (/U, U /)))    STOP 13
  IF (ANY( Ptr(L:U, L)%ID      .NE.   1))          STOP 14
  IF (ANY( Ptr(L:U, L+1:U)%ID  .NE.  -1))          STOP 15

 ! DEALLOCATE(Ptr)

  CONTAINS

  SUBROUTINE S(Ptr, Arr, N, L, U)
  CLASS(DT(:,4)), POINTER :: Ptr(:, :)
  CLASS(DT(*,4)), TARGET  :: Arr(*)
  INTEGER            :: N, L, U
 
  Ptr(L:U, L:U) => Arr(1:N*N)  

  IF (.NOT. ASSOCIATED(Ptr))                       STOP 21
  IF (ANY( LBOUND(Ptr)         .NE. (/L, L /)))    STOP 22
  IF (ANY( UBOUND(Ptr)         .NE. (/U, U /)))    STOP 23
  SELECT TYPE (Ptr)
  TYPE IS (DT(*,4))
    IF (ANY( Ptr%ID            .NE.   -1))         STOP 24
    IF (ANY( Ptr%ModFun()      .NE.   -1))         STOP 25
    Ptr(L:U, L)%ID = 1
  CLASS DEFAULT
    STOP 26
  END SELECT


  END SUBROUTINE

  END



