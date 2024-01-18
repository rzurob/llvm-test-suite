!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefInqTINY.f
!*
!*  DATE                       : Apr. 04, 2006
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
!*  a reference to a specification inquiry
!*
!*  -  TINY
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM   InitExpDefInqTINY
  IMPLICIT NONE
  INTEGER :: I, J, K


  REAL(4),   PARAMETER :: R4 = 10
  REAL(8),   PARAMETER :: R8(-2147483648:-2147483647, 2147483646:2147483647) = 1
  REAL(16),  PARAMETER :: R6(1:0) = -1

  REAL(KIND(TINY(R4)))  :: TR4= TINY(R4)
  REAL(KIND(TINY(R8)))  :: TR8= TINY(R8)
  REAL(KIND(TINY(R6)))  :: TR6= TINY(R6)

  REAL(KIND(TINY(TR4)))  :: TT4= 2.0_4 **(MINEXPONENT(R4)-1)
  REAL(KIND(TINY(TR8)))  :: TT8= 2.0_8 **(MINEXPONENT(R8)-1)
  REAL(KIND(TINY(TR6)))  :: TT6= 2.0_16**(MINEXPONENT(R6)-1)


  IF (  KIND(TR4)  .NE. 4   )   STOP 11
  IF (  TR4        .NE. TT4 )   STOP 12
  IF (  KIND(TR8)  .NE. 8   )   STOP 13
  IF (  TR8        .NE. TT8 )   STOP 14
  IF (  KIND(TR6)  .NE. 16  )   STOP 15
  IF (  TR6        .NE. TT6 )   STOP 16


  END



