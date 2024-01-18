!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefInqIEEE_SUPPORT_HALTING.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 04, 2006
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
!*  a reference to an IEEE inquiry function
!* 
!*  -  IEEE_SUPPORT_HALTING 
!* 
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefInqIEEE_SUPPORT_HALTING 
  USE IEEE_EXCEPTIONS 
  IMPLICIT NONE
  INTEGER :: I, J, K


  TYPE(IEEE_FLAG_TYPE), PARAMETER :: INVALID        = IEEE_INVALID 
  TYPE(IEEE_FLAG_TYPE), PARAMETER :: OVERFLOW       = IEEE_OVERFLOW 
  TYPE(IEEE_FLAG_TYPE), PARAMETER :: DIVIDE_BY_ZERO = IEEE_DIVIDE_BY_ZERO 
  TYPE(IEEE_FLAG_TYPE), PARAMETER :: UNDERFLOW      = IEEE_UNDERFLOW  
  TYPE(IEEE_FLAG_TYPE), PARAMETER :: INEXACT        = IEEE_INEXACT 


  LOGICAL  :: T11= IEEE_SUPPORT_HALTING(INVALID)
  LOGICAL  :: T12= IEEE_SUPPORT_HALTING(OVERFLOW)
  LOGICAL  :: T13= IEEE_SUPPORT_HALTING(DIVIDE_BY_ZERO)
  LOGICAL  :: T14= IEEE_SUPPORT_HALTING(UNDERFLOW)
  LOGICAL  :: T15= IEEE_SUPPORT_HALTING(INEXACT)

  LOGICAL  :: T21= IEEE_SUPPORT_HALTING(IEEE_INVALID)
  LOGICAL  :: T22= IEEE_SUPPORT_HALTING(IEEE_OVERFLOW)
  LOGICAL  :: T23= IEEE_SUPPORT_HALTING(IEEE_DIVIDE_BY_ZERO)
  LOGICAL  :: T24= IEEE_SUPPORT_HALTING(IEEE_UNDERFLOW)
  LOGICAL  :: T25= IEEE_SUPPORT_HALTING(IEEE_INEXACT)



  IF ( .NOT.  T11  )   STOP 11
  IF ( .NOT.  T12  )   STOP 12
  IF ( .NOT.  T13  )   STOP 13
  IF ( .NOT.  T14  )   STOP 14
  IF ( .NOT.  T15  )   STOP 15

  IF ( .NOT.  T21  )   STOP 21
  IF ( .NOT.  T22  )   STOP 22
  IF ( .NOT.  T23  )   STOP 23
  IF ( .NOT.  T24  )   STOP 24
  IF ( .NOT.  T25  )   STOP 25


  END


 
