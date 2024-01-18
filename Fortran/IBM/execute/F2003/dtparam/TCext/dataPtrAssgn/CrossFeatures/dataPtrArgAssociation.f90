! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrArgAssociation.f
! opt variations: -ql

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrArgAssociation.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 16, 2006
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
!*  Argument  
!*
!*  
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    INTEGER(K1)   :: ID
  END TYPE

  TYPE, EXTENDS(DT) :: DT1    ! (4)
  END TYPE


  END MODULE

  PROGRAM dataPtrArgAssociation 
  USE M
  IMPLICIT NONE

  TYPE(DT(4)),  TARGET  :: Tar2(100, 100)
  TYPE(DT1(4)), TARGET  :: Tar1(10000)
  CLASS(DT(4)), POINTER :: Ptr(:, :)
  INTEGER    :: I, J, K, N

  INTERFACE
    SUBROUTINE  ExtSub2(Ptr, Arr, I, J, N)
    IMPORT DT 
      TYPE(DT(4)),  TARGET  :: Arr(:, :)
      CLASS(DT(4)), POINTER :: Ptr(:, :)
      INTEGER            :: I, J, N
    END SUBROUTINE

    SUBROUTINE  ExtSub1(Ptr, Arr, I, J, N)
    IMPORT DT1, DT 
      TYPE(DT1(4)), TARGET  :: Arr(:)
      CLASS(DT(4)), POINTER :: Ptr(:,:)
      INTEGER            :: I, J, N
    END SUBROUTINE

  END INTERFACE

  N = 100; K = 0

  DO I =1, N 
  DO J =I, N

    Ptr(I:, J:) => Tar2
    CALL ExtSub2(Ptr, Tar2, I, J, N)

    SELECT TYPE(Ptr)
    TYPE IS (DT(4))
      Ptr = DT(4)(ID=I*J)
    END SELECT
 
    IF (SIZE(Ptr)  .NE. N*N )                      STOP 10
    IF (.NOT. ASSOCIATED(Ptr, Tar2))               STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/-I, -J /)))        STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/-I+N-1, -J+N-1/))) STOP 13
    IF (ANY( Tar2%ID     .NE.  I*J ))              STOP 14
 
    Ptr(I:J, I:J) => Tar1
    CALL ExtSub1(Ptr, Tar1, I, J, N) 

    SELECT TYPE (Ptr)
    TYPE IS (DT1(4))
      Ptr = DT1(4)(ID=-I*J) 
    END SELECT

    IF (SIZE(Ptr)  .NE. (J-I+1)*(J-I+1))               STOP 20
    IF (.NOT. ASSOCIATED(Ptr))                         STOP 21
    IF (ANY( LBOUND(Ptr) .NE. (/-J,  -J/)))            STOP 22
    IF (ANY( UBOUND(Ptr) .NE. (/-I,  -I/)))            STOP 23
    IF (ANY( Tar1(1:(J-I+1)*(J-I+1))%ID .NE.  -I*J ))  STOP 24
 
  END DO
  END DO


  END

  SUBROUTINE  ExtSub1(Ptr, Arr, I, J, N)
  USE M
  TYPE(DT1(4)), TARGET  :: Arr(:)
  CLASS(DT(4)), POINTER :: Ptr(:,:)
  INTEGER            :: I, J, N

    IF (SIZE(Ptr)  .NE. (J-I+1)*(J-I+1))         STOP 30
    IF (ANY( LBOUND(Ptr) .NE. (/I, I /)))        STOP 32
    IF (ANY( UBOUND(Ptr) .NE. (/J, J /)))        STOP 33
    Ptr(-J:-I, -j:-I) => Arr

  END SUBROUTINE

  SUBROUTINE  ExtSub2(Ptr, Arr, I, J, N)
  USE M
  TYPE(DT(4)),  TARGET  :: Arr(:, :)
  CLASS(DT(4)), POINTER :: Ptr(:, :)
  INTEGER            :: I, J, N

    IF (SIZE(Ptr)  .NE. N*N )                    STOP 40
    IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))        STOP 42
    IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/))) STOP 43
    Ptr(-I:, -J:) => Arr

  END SUBROUTINE

