!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 05, 2006
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
!*  a reference to an IEEE inquiry function
!*
!*  -  IEEE_SUPPORT_DATATYPE
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefInqIEEE_SUPPORT_DATATYPE
  USE IEEE_ARITHMETIC
  IMPLICIT NONE
  INTEGER :: I, J, K


  REAL(4),   PARAMETER :: R4 = 10
  REAL(8),   PARAMETER :: R8(-2147483648:-2147483647, 2147483646:2147483647) = 1
  REAL(16),  PARAMETER :: R6(1:0) = -1


  LOGICAL  :: T11= IEEE_SUPPORT_DATATYPE(R4)
  LOGICAL  :: T12= IEEE_SUPPORT_DATATYPE(R8)
  LOGICAL  :: T13= IEEE_SUPPORT_DATATYPE(R6)

  LOGICAL  :: T21= IEEE_SUPPORT_DATATYPE((/0._4/))
  LOGICAL  :: T22= IEEE_SUPPORT_DATATYPE(HUGE(8._8))
  LOGICAL  :: T23= IEEE_SUPPORT_DATATYPE((/HUGE(1._16)/))

  LOGICAL  :: T  = IEEE_SUPPORT_DATATYPE()



  IF ( .NOT.  T11  )   ERROR STOP 11
  IF ( .NOT.  T12  )   ERROR STOP 12
  IF (        T13  )   ERROR STOP 13

  IF ( .NOT.  T21  )   ERROR STOP 21
  IF ( .NOT.  T22  )   ERROR STOP 22
  IF (        T23  )   ERROR STOP 23

  IF (        T    )   ERROR STOP 33

  END


