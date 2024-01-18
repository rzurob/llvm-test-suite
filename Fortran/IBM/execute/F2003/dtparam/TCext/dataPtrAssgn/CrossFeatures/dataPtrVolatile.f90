! GB DTP extension using:
! ftcx_dtp -qck -qdeferredlp /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrVolatile.f
! opt variations: -qnock -qnodeferredlp -qreuse=self

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrVolatile.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 15, 2006
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
!*  Volatile 
!*
!*  
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT(K1,N1,K2,K3,N2)    ! (1,1,4,1,1)
    INTEGER, KIND             :: K1,K2,K3
    INTEGER, LEN              :: N1,N2
    CHARACTER(kind=K1,len=N1) :: C0="!" 
    INTEGER(K2)               :: I=0
    CHARACTER(kind=K3,len=N2) :: C1="!"
  END TYPE

  END MODULE

  PROGRAM dataPtrVolatile 
  USE M
  IMPLICIT NONE

  TYPE(DT(1,1,4,1,1)),  VOLATILE , TARGET  :: Arr(100, 100), Arr1(10000), T(100,100)
  CLASS(DT(1,:,4,1,:)), VOLATILE , POINTER :: Ptr(:, :)
  INTEGER             :: I, J


  DO I =1, 100 
  DO J =I, 100 

    Arr(I:, J:) = DT(1,1,4,1,1)(I=-I)

    Ptr(I:, J:) => Arr(I:, J:) 
    IF (.NOT. ASSOCIATED(Ptr, Arr(I:, J:) ))     STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/I , J/)))       STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/100, 100  /)))  STOP 13
    T(I:, J:) = Ptr(I:, J:)
    IF (ANY(T(I:, J:)%I  .NE. -I ))             STOP 14

    Arr1(1:(J-I+1)*(J-I+1)) = DT(1,1,4,1,1)(I=-I)
    Ptr(I:J, I:J) => Arr1 
    IF (.NOT. ASSOCIATED(Ptr))                  STOP 21
    IF (SIZE(Ptr) .NE. (J-I+1)*(J-I+1))         STOP 21
    IF (ANY( LBOUND(Ptr) .NE. (/I , I /)))      STOP 22
    IF (ANY( UBOUND(Ptr) .NE. (/J , J /)))      STOP 23
    T(I:J, I:J) = Ptr(I:J, I:J)
    IF (ANY( T(I:J, I:J)%I .NE. -I ))           STOP 24

  END DO
  END DO


  END


