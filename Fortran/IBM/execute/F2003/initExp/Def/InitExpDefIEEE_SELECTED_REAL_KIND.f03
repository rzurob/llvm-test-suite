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
!*  -  IEEE_SELECTED_REAL_KIND
!*  (319319)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM   InitExpDefIEEE_SELECTED_REAL_KIND
  USE IEEE_ARITHMETIC
  IMPLICIT NONE
  INTEGER :: I, J, K


  INTEGER  :: R(12) = (/4,8,-2,8,8,-2,16,-4,-2,-1,-1,-3/)

  INTEGER(1), PARAMETER :: PI11=6
  INTEGER(1), PARAMETER :: PI12=14
  INTEGER(1), PARAMETER :: PI13=30
  INTEGER(1), PARAMETER :: PI14=63
  INTEGER(2), PARAMETER :: RI11=36
  INTEGER(2), PARAMETER :: RI12=300
  INTEGER(2), PARAMETER :: RI13=999

  INTEGER, PARAMETER :: T1(12) = (/                           &
                        IEEE_SELECTED_REAL_KIND(PI11, RI11)   &
                      , IEEE_SELECTED_REAL_KIND(PI11, RI12)   &
                      , IEEE_SELECTED_REAL_KIND(PI11, RI13)   &
                      , IEEE_SELECTED_REAL_KIND(PI12, RI11)   &
                      , IEEE_SELECTED_REAL_KIND(PI12, RI12)   &
                      , IEEE_SELECTED_REAL_KIND(PI12, RI13)   &
                      , IEEE_SELECTED_REAL_KIND(PI13, RI11)   &
                      , IEEE_SELECTED_REAL_KIND(PI13, RI12)   &
                      , IEEE_SELECTED_REAL_KIND(PI13, RI13)   &
                      , IEEE_SELECTED_REAL_KIND(PI14, RI11)   &
                      , IEEE_SELECTED_REAL_KIND(PI14, RI12)   &
                      , IEEE_SELECTED_REAL_KIND(PI14, RI13)/)


  INTEGER(2), PARAMETER :: PI21=6
  INTEGER(2), PARAMETER :: PI22=14
  INTEGER(2), PARAMETER :: PI23=30
  INTEGER(2), PARAMETER :: PI24=63
  INTEGER(2), PARAMETER :: RI21=36
  INTEGER(2), PARAMETER :: RI22=300
  INTEGER(2), PARAMETER :: RI23=999

  INTEGER, PARAMETER :: T2(12) = (/                           &
                        IEEE_SELECTED_REAL_KIND(PI21, RI21)   &
                      , IEEE_SELECTED_REAL_KIND(PI21, RI22)   &
                      , IEEE_SELECTED_REAL_KIND(PI21, RI23)   &
                      , IEEE_SELECTED_REAL_KIND(PI22, RI21)   &
                      , IEEE_SELECTED_REAL_KIND(PI22, RI22)   &
                      , IEEE_SELECTED_REAL_KIND(PI22, RI23)   &
                      , IEEE_SELECTED_REAL_KIND(PI23, RI21)   &
                      , IEEE_SELECTED_REAL_KIND(PI23, RI22)   &
                      , IEEE_SELECTED_REAL_KIND(PI23, RI23)   &
                      , IEEE_SELECTED_REAL_KIND(PI24, RI21)   &
                      , IEEE_SELECTED_REAL_KIND(PI24, RI22)   &
                      , IEEE_SELECTED_REAL_KIND(PI24, RI23)/)


  INTEGER(2), PARAMETER :: PI41=6
  INTEGER(2), PARAMETER :: PI42=14
  INTEGER(2), PARAMETER :: PI43=30
  INTEGER(2), PARAMETER :: PI44=63
  INTEGER(2), PARAMETER :: RI41=36
  INTEGER(2), PARAMETER :: RI42=300
  INTEGER(2), PARAMETER :: RI43=999

  INTEGER, PARAMETER :: T4(12) = (/                           &
                        IEEE_SELECTED_REAL_KIND(PI41, RI41)   &
                      , IEEE_SELECTED_REAL_KIND(PI41, RI42)   &
                      , IEEE_SELECTED_REAL_KIND(PI41, RI43)   &
                      , IEEE_SELECTED_REAL_KIND(PI42, RI41)   &
                      , IEEE_SELECTED_REAL_KIND(PI42, RI42)   &
                      , IEEE_SELECTED_REAL_KIND(PI42, RI43)   &
                      , IEEE_SELECTED_REAL_KIND(PI43, RI41)   &
                      , IEEE_SELECTED_REAL_KIND(PI43, RI42)   &
                      , IEEE_SELECTED_REAL_KIND(PI43, RI43)   &
                      , IEEE_SELECTED_REAL_KIND(PI44, RI41)   &
                      , IEEE_SELECTED_REAL_KIND(PI44, RI42)   &
                      , IEEE_SELECTED_REAL_KIND(PI44, RI43)/)


  INTEGER(2), PARAMETER :: PI81=6
  INTEGER(2), PARAMETER :: PI82=14
  INTEGER(2), PARAMETER :: PI83=30
  INTEGER(2), PARAMETER :: PI84=63
  INTEGER(2), PARAMETER :: RI81=36
  INTEGER(2), PARAMETER :: RI82=300
  INTEGER(2), PARAMETER :: RI83=999

  INTEGER, PARAMETER :: T8(12) = (/                           &
                        IEEE_SELECTED_REAL_KIND(PI81, RI81)   &
                      , IEEE_SELECTED_REAL_KIND(PI81, RI82)   &
                      , IEEE_SELECTED_REAL_KIND(PI81, RI83)   &
                      , IEEE_SELECTED_REAL_KIND(PI82, RI81)   &
                      , IEEE_SELECTED_REAL_KIND(PI82, RI82)   &
                      , IEEE_SELECTED_REAL_KIND(PI82, RI83)   &
                      , IEEE_SELECTED_REAL_KIND(PI83, RI81)   &
                      , IEEE_SELECTED_REAL_KIND(PI83, RI82)   &
                      , IEEE_SELECTED_REAL_KIND(PI83, RI83)   &
                      , IEEE_SELECTED_REAL_KIND(PI84, RI81)   &
                      , IEEE_SELECTED_REAL_KIND(PI84, RI82)   &
                      , IEEE_SELECTED_REAL_KIND(PI84, RI83)/)

  LOGICAL :: LI11 = IEEE_SELECTED_REAL_KIND(PI11)   .EQ. IEEE_SELECTED_REAL_KIND(PI11, 0_1)
  LOGICAL :: LI12 = IEEE_SELECTED_REAL_KIND(R=RI11) .EQ. IEEE_SELECTED_REAL_KIND(0_1, RI11)

  LOGICAL :: LI21 = IEEE_SELECTED_REAL_KIND(PI21)   .EQ. IEEE_SELECTED_REAL_KIND(PI21, 0_2)
  LOGICAL :: LI22 = IEEE_SELECTED_REAL_KIND(R=RI21) .EQ. IEEE_SELECTED_REAL_KIND(0_2, RI21)

  LOGICAL :: LI41 = IEEE_SELECTED_REAL_KIND(PI41)   .EQ. IEEE_SELECTED_REAL_KIND(PI41, 0_4)
  LOGICAL :: LI42 = IEEE_SELECTED_REAL_KIND(R=RI41) .EQ. IEEE_SELECTED_REAL_KIND(0_4, RI41)

  LOGICAL :: LI81 = IEEE_SELECTED_REAL_KIND(PI81)   .EQ. IEEE_SELECTED_REAL_KIND(PI81, 0_8)
  LOGICAL :: LI82 = IEEE_SELECTED_REAL_KIND(R=RI81) .EQ. IEEE_SELECTED_REAL_KIND(0_8, RI81)


! IF( ANY( T1  .NE. R ))  ERROR STOP 11  ! integer(1) can not contains num > 127
  IF( ANY( T2  .NE. R ))  ERROR STOP 12
  IF( ANY( T4  .NE. R ))  ERROR STOP 14
  IF( ANY( T8  .NE. R ))  ERROR STOP 18


  IF ( .NOT. LI11  )  ERROR STOP 21
  IF ( .NOT. LI12  )  ERROR STOP 22
  IF ( .NOT. LI21  )  ERROR STOP 23
  IF ( .NOT. LI22  )  ERROR STOP 24
  IF ( .NOT. LI41  )  ERROR STOP 25
  IF ( .NOT. LI42  )  ERROR STOP 26
  IF ( .NOT. LI81  )  ERROR STOP 27
  IF ( .NOT. LI82  )  ERROR STOP 28



  END



