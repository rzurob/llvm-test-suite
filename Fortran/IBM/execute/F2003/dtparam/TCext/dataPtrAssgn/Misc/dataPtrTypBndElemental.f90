! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/Misc/dataPtrTypBndElemental.f
! opt variations: -qnol -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jul. 31, 2006
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
!*  Elemental - type bound proc
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
    PROCEDURE :: Fun => ModFun
    PROCEDURE :: Sub => ModSub
  END TYPE

  CONTAINS

  ELEMENTAL FUNCTION ModFun(Arg)
  INTEGER :: ModFun
  CLASS(DT(*,4)), INTENT(IN) :: Arg
    ModFun = Arg%ID
  END FUNCTION

  ELEMENTAL SUBROUTINE ModSub(Arg)
  CLASS(DT(*,4)), INTENT(INOUT) :: Arg
    Arg%ID = Arg%ID+1
  END SUBROUTINE

  END MODULE


  PROGRAM dataPtrTypBndElemental
  USE M
  IMPLICIT NONE

  INTEGER    :: I, J, K, N

  TYPE(DT(20,4)), TARGET  :: Tar1(1000)=(/(DT(20,4)(ID=I), I=1,1000)/)
  TYPE(DT(20,4)), TARGET  :: Tar2(100,10)=RESHAPE((/(DT(20,4)(ID=I), I=1,1000)/),(/100,10/))
  TYPE(DT(:,4)),  POINTER :: Ptr(:, :)
  TYPE(DT(20,4))  ::      T1(100)=DT(20,4)(ID=0)
  TYPE(DT(20,4))  ::      T2(10,10)=DT(20,4)(ID=0)


  Ptr(1:,1:) => Tar2
  IF (.NOT. ASSOCIATED(Ptr, Tar2))                                            STOP 11
  IF (ANY( LBOUND(Ptr) .NE. (/1, 1 /)))                                       STOP 12
  IF (ANY( UBOUND(Ptr) .NE. (/100,10/)))                                      STOP 13
  IF (ANY( Ptr%ID      .NE. RESHAPE((/(I, I=1,1000)/),(/100,10/))))           STOP 14
  IF (ANY( Ptr%Fun()   .NE. RESHAPE((/(I, I=1,1000)/),(/100,10/))))           STOP 15

  CALL Ptr%Sub()
  IF (ANY( Ptr%ID      .NE. RESHAPE((/(I+1, I=1,1000)/),(/100,10/))))         STOP 16

  Ptr(0:,0:) => Tar2
  IF (.NOT. ASSOCIATED(Ptr, Tar2))                                            STOP 21
  IF (ANY( LBOUND(Ptr) .NE. (/0, 0 /)))                                       STOP 22
  IF (ANY( UBOUND(Ptr) .NE. (/99,9/)))                                        STOP 23
  IF (ANY( Ptr%ID      .NE. RESHAPE((/(I+1, I=1,1000)/),(/100,10/))))         STOP 24
  IF (ANY( Ptr%Fun()   .NE. RESHAPE((/(I+1, I=1,1000)/),(/100,10/))))         STOP 25

  CALL Ptr%Sub()
  IF (ANY( Ptr%ID      .NE. RESHAPE((/(I+2, I=1,1000)/),(/100,10/))))         STOP 26


  Ptr(0:9,0:9) => Tar1
  IF (.NOT. ASSOCIATED(Ptr))                                                STOP 31
  IF (ANY( LBOUND(Ptr) .NE. (/0, 0 /)))                                     STOP 32
  IF (ANY( UBOUND(Ptr) .NE. (/9,9/)))                                       STOP 33
  IF (ANY( Ptr%ID      .NE. RESHAPE((/(I, I=1,100)/),(/10,10/))))           STOP 34
  IF (ANY( Ptr%Fun()   .NE. RESHAPE((/(I, I=1,100)/),(/10,10/))))           STOP 35

  CALL Ptr%Sub()
  IF (ANY( Ptr%ID      .NE. RESHAPE((/(I+1, I=1,100)/),(/10,10/))))         STOP 36

  END



