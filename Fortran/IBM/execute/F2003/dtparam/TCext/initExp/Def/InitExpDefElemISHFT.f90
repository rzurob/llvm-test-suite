! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=none /tstdev/F2003/initExp/Def/InitExpDefElemISHFT.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=self

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 10, 2006
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
!*  -  ISHFT
!*  (319057)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemISHFT
  IMPLICIT  NONE
  INTEGER :: I, J

  INTEGER(KIND(ISHFT((/(1_1, I=0, 127)/), 2_8))),    PARAMETER ::    &
      I11(128)=ISHFT((/(1_1, I=0, 127)/), 2_8)
  INTEGER(KIND(ISHFT((/(-128_1, I=0, 127)/), -2))),  PARAMETER ::    &
      I12(128)=ISHFT((/(-128_1, I=0, 127)/), -2)
  INTEGER(KIND(ISHFT((/(-128_1, I=0, 127)/), 0_8))), PARAMETER ::    &
      I13(128)=ISHFT((/(-128_1, I=0, 127)/), 0_8)
  INTEGER(KIND(ISHFT((/(1_1, I=0, 127)/), (/(1,-1, I=1,128,2)/)))), PARAMETER ::    &
      I14(128)=ISHFT(I=(/(1_1, I=0, 127)/), SHIFT=(/(1,-1, I=1,128,2)/))
  INTEGER(1),  PARAMETER :: I1(128)=(/(2,0, I=1,128,2)/)


  TYPE :: DT0(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: I(128)
  END TYPE

  TYPE :: DT(K2,N2)    ! (4,20)
    INTEGER, KIND    :: K2
    INTEGER, LEN     :: N2
    TYPE(DT0(N2,K2)) :: Arr(4)
  END TYPE

  TYPE(DT(4,20)), PARAMETER :: T=DT(4,20)((/DT0(20,4)(ISHFT((/(1_1, I=0, 127)/), 2_8)),    &
                                DT0(20,4)(ISHFT((/(-128_1, I=0, 127)/), -2)),  &
                                DT0(20,4)(ISHFT((/(-128_1, I=0, 127)/), 0_8)), &
                                DT0(20,4)(ISHFT(I=(/(1_1, I=0, 127)/), SHIFT=(/(1,-1, I=1,128,2)/)) &
                              )/))

  IF ( KIND(I11) .NE. 1 )         STOP 11
  IF ( ANY( I11  .NE. 4 ))        STOP 12
  IF ( KIND(I12) .NE. 1 )         STOP 13
  IF ( ANY( I12  .NE. 32 ))       STOP 14
  IF ( KIND(I13) .NE. 1 )         STOP 15
  IF ( ANY( I13  .NE. -128 ))     STOP 16
  IF ( ANY( I14  .NE. I1 ))       STOP 17


  IF ( ANY( T%Arr(1)%I  .NE. 4 ))        STOP 22
  IF ( ANY( T%Arr(2)%I  .NE. 32 ))       STOP 24
  IF ( ANY( T%Arr(3)%I  .NE. -128 ))     STOP 26
  IF ( ANY( T%Arr(4)%I  .NE. I1 ))       STOP 27

  END


