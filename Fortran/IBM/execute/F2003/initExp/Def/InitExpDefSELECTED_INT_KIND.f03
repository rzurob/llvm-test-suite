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
!*  -  SELECTED_INT_KIND
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM   InitExpDefSELECTED_INT_KIND
  IMPLICIT NONE
  INTEGER :: I, J, K


  INTEGER, PARAMETER :: TK0       = SELECTED_INT_KIND(0)
  INTEGER, PARAMETER :: TK2       = SELECTED_INT_KIND(2)
  INTEGER, PARAMETER :: TK4       = SELECTED_INT_KIND(4)
  INTEGER, PARAMETER :: TK8       = SELECTED_INT_KIND(8)
  INTEGER, PARAMETER :: TK16      = SELECTED_INT_KIND(16)
  INTEGER, PARAMETER :: TK32      = SELECTED_INT_KIND(32)
  INTEGER, PARAMETER :: TKX(33)   =(/(SELECTED_INT_KIND(I) , I=0,32)/)
  INTEGER, PARAMETER :: Check(33) =    &
       (/1,1,1,2,2,4,4,4,4,4,8,8,8,8,8,8,8,8,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/)
  INTEGER, PARAMETER :: TK        =KIND((/(SELECTED_INT_KIND(I) , I=0,32)/))

  IF (TK0             .NE.   1 )                   ERROR STOP 10
  IF (TK2             .NE.   1 )                   ERROR STOP 11
  IF (TK4             .NE.   2 )                   ERROR STOP 12
  IF (TK8             .NE.   4 )                   ERROR STOP 14
  IF (TK16            .NE.   8 )                   ERROR STOP 15
  IF (TK32            .NE.   -1 )                  ERROR STOP 16
  IF (ANY(TKX         .NE.   CHECK) )              ERROR STOP 17
  IF (TK              .NE.   4 )                   ERROR STOP 18

  END



