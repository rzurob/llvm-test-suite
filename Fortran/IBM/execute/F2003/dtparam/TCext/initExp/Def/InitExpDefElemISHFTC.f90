! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=self /tstdev/F2003/initExp/Def/InitExpDefElemISHFTC.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=none

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemISHFTC.f
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
!*  -  ISHFTC
!*  (319093/319591)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemISHFTC
  IMPLICIT  NONE
  INTEGER :: I, J

  INTEGER(KIND(ISHFTC((/(1_1, I=0, 127)/), SIZE=8_8, SHIFT=8))),    PARAMETER ::    &
       I1(128)=ISHFTC((/(1_1, I=0, 127)/), SIZE=8_8, SHIFT=8)
  INTEGER(KIND(ISHFTC((/(-128_2, I=0, 127)/), SHIFT=-2, SIZE=2))),  PARAMETER ::    &
       I2(128)=ISHFTC((/(-128_2, I=0, 127)/), SHIFT=-2, SIZE=2)
  INTEGER(KIND(ISHFTC((/(-128_4, I=0, 127)/), 0_8, SIZE=32))),      PARAMETER ::    &
       I4(128)=ISHFTC((/(-128_4, I=0, 127)/), 0_8, SIZE=32)
  INTEGER(KIND(ISHFTC(I=(/(1_8, I=0, 127)/), SHIFT=(/(1,-1, I=1,128,2)/), SIZE=(/(1, I=1,128)/)))), PARAMETER ::    &
       I8(128)=ISHFTC(I=(/(1_8, I=0, 127)/), SHIFT=(/(1,-1, I=1,128,2)/), SIZE=(/(1, I=1,128)/))


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
                                !DT0(ISHFTC((/(INT(z"FFF0", 2), I=0, 127)/), SHIFT=-4, SIZE=2)),    &

  TYPE(DT(4,20)), PARAMETER :: T=DT(4,20)((/DT0(20,4)(ISHFTC((/(1_1, I=0, 127)/), SIZE=8_8, SHIFT=8)),      &
                                DT0(20,4)(ISHFTC((/(-128_2, I=0, 127)/), SHIFT=-2, SIZE=2)),    &
                                DT0(20,4)(ISHFTC((/(-128_4, I=0, 127)/), 0_8, SIZE=32)),        &
                                DT0(20,4)(ISHFTC(I=(/(1_8, I=0, 127)/), SHIFT=(/(1,-1, I=1,128,2)/), SIZE=(/(1, I=1,128)/))) &
                              /))

  IF ( KIND(I1) .NE. 1 )         STOP 11
  IF ( ANY( I1  .NE. 1 ))        STOP 12
  IF ( KIND(I2) .NE. 2 )         STOP 13
  IF ( ANY( I2  .NE. -128 ))     STOP 14
  IF ( KIND(I4) .NE. 4 )         STOP 15
  IF ( ANY( I4  .NE. -128 ))     STOP 16
  IF ( KIND(I8) .NE. 8 )         STOP 17
  IF ( ANY( I8  .NE. 1 ))        STOP 18


  IF ( ANY( T%Arr(1)%I  .NE. 1 ))        STOP 21
  IF ( ANY( T%Arr(2)%I  .NE. -128 ))     STOP 22
  IF ( ANY( T%Arr(3)%I  .NE. -128 ))     STOP 23
  IF ( ANY( T%Arr(4)%I  .NE. 1 ))        STOP 24

  END


