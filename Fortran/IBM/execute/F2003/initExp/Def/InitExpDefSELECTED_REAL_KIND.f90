!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 30, 2006
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
!*  -  SELECTED_REAL_KIND
!*  (319319)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM   InitExpDefSELECTED_REAL_KIND
  IMPLICIT NONE
  INTEGER :: I, J, K


  INTEGER  :: R(12) = (/4, 8, -2, 8, 8, -2, 16, 16, -2, -1, -1, -3/)

  INTEGER(1), PARAMETER :: P11 =6
  INTEGER(1), PARAMETER :: P12=14
  INTEGER(1), PARAMETER :: P13=30
  INTEGER(1), PARAMETER :: P14=33

  INTEGER(2), PARAMETER :: R1=36
  INTEGER(2), PARAMETER :: R2=291  ! for real(16)
  INTEGER(2), PARAMETER :: R3=999

  INTEGER, PARAMETER :: T1(12) = (/                   &
                        SELECTED_REAL_KIND(P11, R1)   &
                      , SELECTED_REAL_KIND(P11, R2)   &
                      , SELECTED_REAL_KIND(P11, R3)   &
                      , SELECTED_REAL_KIND(P12, R1)   &
                      , SELECTED_REAL_KIND(P12, R2)   &
                      , SELECTED_REAL_KIND(P12, R3)   &
                      , SELECTED_REAL_KIND(P13, R1)   &
                      , SELECTED_REAL_KIND(P13, R2)   &
                      , SELECTED_REAL_KIND(P13, R3)   &
                      , SELECTED_REAL_KIND(P14, R1)   &
                      , SELECTED_REAL_KIND(P14, R2)   &
                      , SELECTED_REAL_KIND(P14, R3)/)

  INTEGER(2), PARAMETER :: P21 =6
  INTEGER(2), PARAMETER :: P22=14
  INTEGER(2), PARAMETER :: P23=30
  INTEGER(2), PARAMETER :: P24=33

  INTEGER, PARAMETER :: T2(12) = (/                   &
                        SELECTED_REAL_KIND(P21, R1)   &
                      , SELECTED_REAL_KIND(P21, R2)   &
                      , SELECTED_REAL_KIND(P21, R3)   &
                      , SELECTED_REAL_KIND(P22, R1)   &
                      , SELECTED_REAL_KIND(P22, R2)   &
                      , SELECTED_REAL_KIND(P22, R3)   &
                      , SELECTED_REAL_KIND(P23, R1)   &
                      , SELECTED_REAL_KIND(P23, R2)   &
                      , SELECTED_REAL_KIND(P23, R3)   &
                      , SELECTED_REAL_KIND(P24, R1)   &
                      , SELECTED_REAL_KIND(P24, R2)   &
                      , SELECTED_REAL_KIND(P24, R3)/)

  INTEGER(4), PARAMETER :: P41 =6
  INTEGER(4), PARAMETER :: P42=14
  INTEGER(4), PARAMETER :: P43=30
  INTEGER(4), PARAMETER :: P44=33

  INTEGER, PARAMETER :: T4(12) = (/                   &
                        SELECTED_REAL_KIND(P41, R1)   &
                      , SELECTED_REAL_KIND(P41, R2)   &
                      , SELECTED_REAL_KIND(P41, R3)   &
                      , SELECTED_REAL_KIND(P42, R1)   &
                      , SELECTED_REAL_KIND(P42, R2)   &
                      , SELECTED_REAL_KIND(P42, R3)   &
                      , SELECTED_REAL_KIND(P43, R1)   &
                      , SELECTED_REAL_KIND(P43, R2)   &
                      , SELECTED_REAL_KIND(P43, R3)   &
                      , SELECTED_REAL_KIND(P44, R1)   &
                      , SELECTED_REAL_KIND(P44, R2)   &
                      , SELECTED_REAL_KIND(P44, R3)/)

  INTEGER(8), PARAMETER :: P81 =6
  INTEGER(8), PARAMETER :: P82=14
  INTEGER(8), PARAMETER :: P83=30
  INTEGER(8), PARAMETER :: P84=33

  INTEGER, PARAMETER :: T8(12) = (/                   &
                        SELECTED_REAL_KIND(P81, R1)   &
                      , SELECTED_REAL_KIND(P81, R2)   &
                      , SELECTED_REAL_KIND(P81, R3)   &
                      , SELECTED_REAL_KIND(P82, R1)   &
                      , SELECTED_REAL_KIND(P82, R2)   &
                      , SELECTED_REAL_KIND(P82, R3)   &
                      , SELECTED_REAL_KIND(P83, R1)   &
                      , SELECTED_REAL_KIND(P83, R2)   &
                      , SELECTED_REAL_KIND(P83, R3)   &
                      , SELECTED_REAL_KIND(P84, R1)   &
                      , SELECTED_REAL_KIND(P84, R2)   &
                      , SELECTED_REAL_KIND(P84, R3)/)



  LOGICAL :: LI11 = SELECTED_REAL_KIND(P11) .EQ. SELECTED_REAL_KIND(P11, 0_1)
  LOGICAL :: LI21 = SELECTED_REAL_KIND(P21) .EQ. SELECTED_REAL_KIND(P21, 0_2)
  LOGICAL :: LI41 = SELECTED_REAL_KIND(P41) .EQ. SELECTED_REAL_KIND(P41, 0_4)
  LOGICAL :: LI81 = SELECTED_REAL_KIND(P81) .EQ. SELECTED_REAL_KIND(P81, 0_8)

  LOGICAL :: LI12 = SELECTED_REAL_KIND(R=R1 ) .EQ. SELECTED_REAL_KIND(0_1, R1 )
  LOGICAL :: LI22 = SELECTED_REAL_KIND(R=R2 ) .EQ. SELECTED_REAL_KIND(0_1, R2 )
  LOGICAL :: LI42 = SELECTED_REAL_KIND(R=R3 ) .EQ. SELECTED_REAL_KIND(0_1, R3 )


  IF ( .NOT. LI11  )  ERROR STOP 21
  IF ( .NOT. LI12  )  ERROR STOP 22
  IF ( .NOT. LI21  )  ERROR STOP 23
  IF ( .NOT. LI22  )  ERROR STOP 24
  IF ( .NOT. LI41  )  ERROR STOP 25
  IF ( .NOT. LI42  )  ERROR STOP 26
  IF ( .NOT. LI81  )  ERROR STOP 27

  IF( ANY( T1  .NE. R ))  ERROR STOP 11
  IF( ANY( T2  .NE. R ))  ERROR STOP 12
  IF( ANY( T4  .NE. R ))  ERROR STOP 14
  IF( ANY( T8  .NE. R ))  ERROR STOP 18


  END



