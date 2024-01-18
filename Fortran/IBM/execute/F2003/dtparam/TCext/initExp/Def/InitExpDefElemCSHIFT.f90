! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/initExp/Def/InitExpDefElemCSHIFT.f
! opt variations: -qnol

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 27, 2006
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
!*  a reference to an tranformational intrinsic
!*
!*  - CSHIFT
!*  (318961/318170/320008)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: I
!   CHARACTER  :: C=""
!   LOGICAL(2) :: L(3,3)=.FALSE.
!   PROCEDURE(), POINTER, NOPASS :: ProcPtr=>NULL()
    CONTAINS
    PROCEDURE  :: Proc => ModSub
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(Arg)
  CLASS(DT(*,4)) :: Arg
  END SUBROUTINE

  END MODULE

  PROGRAM  InitExpDefElemCSHIFT
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER(1),   PARAMETER :: M11(6)=(/(I, I=1,6)/)
  INTEGER(2),   PARAMETER :: M12(6)=(/(I, I=1,6)/)
  INTEGER(4),   PARAMETER :: M14(6)=(/(I, I=1,6)/)
  INTEGER(8),   PARAMETER :: M18(6)=(/(I, I=1,6)/)

  TYPE(DT(20,4)),   PARAMETER :: M2(6)=(/(DT(20,4)(I=I), I=1,6)/)
  TYPE(DT(20,4)),   PARAMETER :: M3(3,3)=RESHAPE(         &
              (/DT(20,4)(1),DT(20,4)(4),DT(20,4)(7),DT(20,4)(2),DT(20,4)(5),DT(20,4)(8),DT(20,4)(3),DT(20,4)(6),DT(20,4)(9)/), (/3,3/))

  TYPE(DT(20,4)),   PARAMETER :: R11(6)=(/DT(20,4)(3),DT(20,4)(4),DT(20,4)(5),DT(20,4)(6),DT(20,4)(1),DT(20,4)(2)/)
  TYPE(DT(20,4)),   PARAMETER :: R12(6)=(/DT(20,4)(5),DT(20,4)(6),DT(20,4)(1),DT(20,4)(2),DT(20,4)(3),DT(20,4)(4)/)
  TYPE(DT(20,4)),   PARAMETER :: M20(3,3)=RESHAPE(         &
              (/DT(20,4)(7),DT(20,4)(1),DT(20,4)(4),DT(20,4)(8),DT(20,4)(2),DT(20,4)(5),DT(20,4)(9),DT(20,4)(3),DT(20,4)(6)/), (/3,3/))
  TYPE(DT(20,4)),   PARAMETER :: M21(3,3)=RESHAPE(         &
              (/DT(20,4)(3),DT(20,4)(6),DT(20,4)(9),DT(20,4)(1),DT(20,4)(4),DT(20,4)(7),DT(20,4)(2),DT(20,4)(5),DT(20,4)(8)/), (/3,3/))
  TYPE(DT(20,4)),   PARAMETER :: M22(3,3)=RESHAPE(         &
              (/DT(20,4)(3),DT(20,4)(5),DT(20,4)(7),DT(20,4)(1),DT(20,4)(6),DT(20,4)(8),DT(20,4)(2),DT(20,4)(4),DT(20,4)(9)/), (/3,3/))


  INTEGER(KIND(CSHIFT(M11, -1))) :: T11(SIZE(CSHIFT(M11, SHIFT=2)))=CSHIFT(M11, SHIFT=2)
  INTEGER(KIND(CSHIFT(M12, -2))) :: T12(SIZE(CSHIFT(M12, SHIFT=2)))=CSHIFT(M12, SHIFT=2)
  INTEGER(KIND(CSHIFT(M14, -4))) :: T14(SIZE(CSHIFT(M14, SHIFT=2)))=CSHIFT(M14, SHIFT=2)
  INTEGER(KIND(CSHIFT(M18, -8))) :: T18(SIZE(CSHIFT(M18, SHIFT=2)))=CSHIFT(M18, SHIFT=2)

  TYPE(DT(20,4)), PARAMETER :: T2(1:0)=DT(20,4)(-1)
  TYPE(DT(20,4)), PARAMETER :: T21(2:1)=CSHIFT(T2, -8)

  TYPE(DT(20,4)) :: T31(3,3)=CSHIFT(CSHIFT(CSHIFT(M3, -1), 1), -1)
  TYPE(DT(20,4)) :: T32(3,3)=CSHIFT(CSHIFT(CSHIFT(M3, 1),  1), SHIFT=-2)

  TYPE(DT(20,4)) :: T41(3, 3)=CSHIFT(CSHIFT(CSHIFT(M3, -1), 1), -1, 2)
  TYPE(DT(20,4)) :: T42(3, 3)=CSHIFT(CSHIFT(CSHIFT(M3, 2), -2), DIM=2, SHIFT=-1)

  INTEGER, PARAMETER  :: SHIFT(3)=(/-1,1,0/)
  TYPE(DT(20,4)) :: T51(3, 3)=CSHIFT(CSHIFT(CSHIFT(M3, -1), 1), SHIFT, 2)
  TYPE(DT(20,4)) :: T52(3, 3)=CSHIFT(CSHIFT(CSHIFT(M3, 2), -2), DIM=2, SHIFT=SHIFT)


  IF (KIND(T11)   .NE.   1 )        ERROR STOP 11
  IF (ANY( T11    .NE.   R11%I ))   ERROR STOP 12
  IF (KIND(T12)   .NE.   2 )        ERROR STOP 13
  IF (ANY( T12    .NE.   R11%I ))   ERROR STOP 14
  IF (KIND(T14)   .NE.   4 )        ERROR STOP 15
  IF (ANY( T14    .NE.   R11%I ))   ERROR STOP 16
  IF (KIND(T18)   .NE.   8 )        ERROR STOP 17
  IF (ANY( T18    .NE.   R11%I ))   ERROR STOP 18

  IF (SIZE(T2 )   .NE.   0 )        ERROR STOP 21
  IF (SIZE(T21)   .NE.   0 )        ERROR STOP 22

  IF (ANY( T31%I  .NE. M20%I ))     ERROR STOP 31
  IF (ANY( T32%I  .NE. M3%I  ))     ERROR STOP 32

  IF (ANY( T41%I  .NE. M21%I ))     ERROR STOP 41
  IF (ANY( T42%I  .NE. M21%I ))     ERROR STOP 42

  IF (ANY( T51%I  .NE. M22%I ))     ERROR STOP 51
  IF (ANY( T52%I  .NE. M22%I ))     ERROR STOP 52

  END



