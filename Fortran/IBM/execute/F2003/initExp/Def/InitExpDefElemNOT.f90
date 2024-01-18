!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemNOT.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 14, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289074 
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
!*  a reference to an elemental intrinsic
!* 
!*  -  NOT 
!*  (319227)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM  InitExpDefElemNOT
  IMPLICIT NONE 
  INTEGER :: I, J

  TYPE :: DT
    INTEGER(1) :: I1(4,4)=RESHAPE((/(INT(z"05", KIND=1), I=1,16)/),(/4,4/)) 
    INTEGER(2) :: I2(4,4)=RESHAPE((/(INT(z"0005", KIND=2), I=1,16)/),(/4,4/)) 
    INTEGER(4) :: I4(4,4)=RESHAPE((/(INT(z"00000005", KIND=4), I=1,16)/),(/4,4/)) 
    INTEGER(8) :: I8(4,4)=RESHAPE((/(INT(z"0000000000000005", KIND=8), I=1,16)/),(/4,4/)) 
  END TYPE

  TYPE (DT), PARAMETER :: X = DT()

  INTEGER :: I1(4,4)=RESHAPE((/(INT(z"FA", KIND=1), I=1,16)/),(/4,4/)) 
  INTEGER :: I2(4,4)=RESHAPE((/(INT(z"FFFA", KIND=2), I=1,16)/),(/4,4/)) 
  INTEGER :: I4(4,4)=RESHAPE((/(INT(z"FFFFFFFA", KIND=4), I=1,16)/),(/4,4/)) 
  INTEGER :: I8(4,4)=RESHAPE((/(INT(z"FFFFFFFFFFFFFFFA", KIND=8), I=1,16)/),(/4,4/))

 
  INTEGER(KIND(NOT(I=X%I1)))    :: TI1(4,4) = NOT(I=X%I1)  ! -6
  INTEGER(KIND(NOT(I=X%I2)))    :: TI2(4,4) = NOT(I=X%I2)
  INTEGER(KIND(NOT(I=X%I4)))    :: TI4(4,4) = NOT(I=X%I4)
  INTEGER(KIND(NOT(I=X%I8)))    :: TI8(4,4) = NOT(I=X%I8)

  !INTEGER :: T1(4,4)=MATMUL(MATRIX_A=NOT(I=X%I1), MATRIX_B=X%I1)
  INTEGER :: T2(4,4)=MATMUL(MATRIX_A=NOT(I=X%I1), MATRIX_B=X%I2)
  INTEGER :: T4(4,4)=MATMUL(MATRIX_A=NOT(I=X%I1), MATRIX_B=X%I4)
  INTEGER :: T8(4,4)=MATMUL(MATRIX_A=NOT(I=X%I1), MATRIX_B=X%I8)

  IF ( KIND(TI1)  .NE.    1 )  STOP 11
  IF ( KIND(TI2)  .NE.    2 )  STOP 12
  IF ( KIND(TI4)  .NE.    4 )  STOP 14
  IF ( KIND(TI8)  .NE.    8 )  STOP 18

  IF (ANY( TI1  .NE. I1 )) STOP 21
  IF (ANY( TI2  .NE. I2 )) STOP 22
  IF (ANY( TI4  .NE. I4 )) STOP 23
  IF (ANY( TI8  .NE. I8 )) STOP 24

  IF ( ANY(T2   .NE. -120 ))  STOP 32
  IF ( ANY(T4   .NE. -120 ))  STOP 33
  IF ( ANY(T8   .NE. -120 ))  STOP 34


  END


