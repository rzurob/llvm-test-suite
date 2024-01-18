!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefInqIEEE_SUPPORT_STANDARD.f
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
!*  -  IEEE_SUPPORT_STANDARD
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM   InitExpDefInqIEEE_SUPPORT_STANDARD
  USE IEEE_ARITHMETIC
  IMPLICIT NONE
  INTEGER :: I, J, K


  REAL(4),   PARAMETER :: R4 = 10
  REAL(8),   PARAMETER :: R8(-2147483648:-2147483647, 2147483646:2147483647) = 1
  REAL(16),  PARAMETER :: R6(1:0) = -1

  REAL(4)  :: T4 = 10
  REAL(8)  :: T8(-2147483648:-2147483647, 2147483646:2147483647) = 1
  REAL(16) :: T6(1:0) = -1

  LOGICAL  :: T41= IEEE_SUPPORT_STANDARD(X=R4)
  LOGICAL  :: T81= IEEE_SUPPORT_STANDARD(R8)
  LOGICAL  :: T61= IEEE_SUPPORT_STANDARD(X=R6)

  LOGICAL  :: T42= IEEE_SUPPORT_STANDARD((/HUGE(0._4)/))
  LOGICAL  :: T82= IEEE_SUPPORT_STANDARD(X=(/TINY(0._8)/))
  LOGICAL  :: T62= IEEE_SUPPORT_STANDARD(16._16)

  LOGICAL  :: T= IEEE_SUPPORT_STANDARD()


  IF ( .NOT.  IEEE_SUPPORT_STANDARD(T4)  )   STOP 11
  IF ( .NOT.  IEEE_SUPPORT_STANDARD(T8)  )   STOP 12
  IF (        IEEE_SUPPORT_STANDARD(T6)  )   STOP 13

  IF ( .NOT.  T41  )   STOP 21
  IF ( .NOT.  T81  )   STOP 22
  IF (        T61  )   STOP 23

  IF ( .NOT.  T42  )   STOP 31
  IF ( .NOT.  T82  )   STOP 32
  IF (        T62  )   STOP 33

  IF (        T   )    STOP 44

  END



