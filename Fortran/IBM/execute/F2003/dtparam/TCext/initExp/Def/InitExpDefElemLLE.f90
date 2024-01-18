! GB DTP extension using:
! ftcx_dtp -qck -ql /tstdev/F2003/initExp/Def/InitExpDefElemLLE.f
! opt variations: -qnock -qnol -qreuse=self

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemLLE.f
!*
!*  DATE                       : Apr. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  a reference to an elemental intrinsic
!*
!*  -  LLE
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemLLE
  IMPLICIT NONE
  INTEGER :: I, J

  TYPE :: DT0(K1,K2,K3,N1,K4,N2)    ! (4,4,1,10,1,10)
    INTEGER, KIND             :: K1,K2,K3,K4
    INTEGER, LEN              :: N1,N2
    INTEGER(K1), POINTER      :: Ptr=>NULL()
    REAL(K2), ALLOCATABLE     :: R
    CHARACTER(kind=K3,len=N1) :: C1=""
    CHARACTER(kind=K4,len=N2) :: C2=ACHAR(1)//ACHAR(2)//ACHAR(3)//ACHAR(4)//ACHAR(5)//   &
                             ACHAR(6)//ACHAR(7)//ACHAR(8)//ACHAR(9)//ACHAR(0)
    CHARACTER(2**8-1)    :: C3=CHAR(0)
    PROCEDURE(), POINTER, NOPASS :: ProcPtr =>NULL()
  END TYPE

  TYPE(DT0(4,4,1,10,1,10)), PARAMETER :: C(16)=DT0(4,4,1,10,1,10)(R=NULL())

  TYPE :: DT(N3,K5,K6,K7,K8,K9,K10,K11)    ! (20,4,4,4,4,4,4,4)
    INTEGER, KIND :: K5,K6,K7,K8,K9,K10,K11
    INTEGER, LEN  :: N3
    LOGICAL(K5)   :: C1(16)=LLE(STRING_B=C%C1, STRING_A=C%C2(1:0))
    LOGICAL(K6)   :: C2(16)=LLE(STRING_B=C%C1, STRING_A=C%C2(10:10))
    LOGICAL(K7)   :: C3(16)=LLE(STRING_B=C%C2, STRING_A=C%C2(:))
    LOGICAL(K8)   :: C4(16)=LLE(STRING_B=C%C2//C%C1, STRING_A=C%C2(:)//C%C2(1:0))
    LOGICAL(K9)   :: C5(16)=LLE(STRING_B=C%C2//C%C3, STRING_A=C%C2(:)//C%C3)
    LOGICAL(K10)  :: C6(16)=LLE(STRING_B=C%C1//C%C3, STRING_A=C%C1(:)//C%C3)
    LOGICAL(K11)  :: C7(16)=LLE(STRING_B=C%C2(1:6),  STRING_A=C%C2(2:6))
  END TYPE

  TYPE(DT(20,4,4,4,4,4,4,4)) :: T

  IF( ANY( T%C1  .NEQV. .TRUE.  ))       STOP 11
  IF( ANY( T%C2  .NEQV. .TRUE. ))       STOP 12
  IF( ANY( T%C3  .NEQV. .TRUE.  ))       STOP 13
  IF( ANY( T%C4  .NEQV. .TRUE.  ))       STOP 14
  IF( ANY( T%C5  .NEQV. .TRUE.  ))       STOP 15
  IF( ANY( T%C6  .NEQV. .TRUE.  ))       STOP 16
  IF( ANY( T%C7  .NEQV. .FALSE. ))       STOP 17


  END


