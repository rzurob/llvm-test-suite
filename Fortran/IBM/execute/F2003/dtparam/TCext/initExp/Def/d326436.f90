! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/initExp/Def/d326436.f
! opt variations: -qnol

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemABS.f
!*
!*  DATE                       : Mar 22, 2006
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
!*  Ref 326436
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM d326436

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    REAL(K1)      :: x
  END TYPE

  TYPE(DT(20,4)) :: A(1) = (/ ( DT(20,4)(I), I = 1, 1 )/)

  PRINT *, A

  IF ( A(1)%X .NE. 1.0 ) STOP 11

  END


