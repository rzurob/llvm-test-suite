!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemCONJG.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar 25, 2006
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
!*  - CONJG 
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemCONJG 
  IMPLICIT NONE
  INTEGER :: I, J, K

  COMPLEX,      PARAMETER :: Z4(31:63,31:63)=(4.0,4.0)
  COMPLEX(8),   PARAMETER :: Z8(31:63,31:63)=(8.0,8.0)
  COMPLEX(16),  PARAMETER :: Z16(31:63,31:63)=(16.0,16.0)

  COMPLEX(KIND(CONJG(Z4))),   PARAMETER  ::   &
        T1(LBOUND(CONJG(Z4),1):UBOUND(CONJG(Z4),1), LBOUND(CONJG(Z4),2):UBOUND(CONJG(Z4),2)) &
        = CONJG(Z4) +Z4

  COMPLEX(KIND(CONJG(Z8))),   PARAMETER  ::   &
        T2(LBOUND(CONJG(Z8),1):UBOUND(CONJG(Z8),1), LBOUND(CONJG(Z8),2):UBOUND(CONJG(Z8),2)) &
        = -CONJG(Z8) - Z8

  COMPLEX(KIND(CONJG(Z16))),   PARAMETER  ::   &
        T3(LBOUND(CONJG(Z16),1):UBOUND(CONJG(Z16),1), LBOUND(CONJG(Z16),2):UBOUND(CONJG(Z16),2)) &
        = CONJG(2*Z16(:,:)) +  2*Z16(63,63) 

  IF (KIND(T1)         .NE. 4 )            STOP 11
  IF (ANY( LBOUND(T1)  .NE. (/1, 1/)))     STOP 12
  IF (ANY( UBOUND(T1)  .NE. (/33, 33/)))   STOP 13
  IF (ANY( T1          .NE. (8.0, 0.0)))   STOP 14

  IF (KIND(T2)         .NE. 8 )            STOP 21
  IF (ANY( LBOUND(T2)  .NE. (/1, 1/)))     STOP 22
  IF (ANY( UBOUND(T2)  .NE. (/33, 33/)))   STOP 23
  IF (ANY( T2          .NE. (-16.0, 0.0))) STOP 24

  IF (KIND(T3)         .NE. 16 )           STOP 31
  IF (ANY( LBOUND(T3)  .NE. (/1, 1/)))     STOP 32
  IF (ANY( UBOUND(T3)  .NE. (/33, 33/)))   STOP 33
  IF (ANY( T3          .NE. (64.0, 0.0)))  STOP 34






  END


 
