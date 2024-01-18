!*********************************************************************
!*  ===================================================================
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
!*  a reference to an elemental intrinsic
!*
!*  - ADJUSTR / KIND
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM InitExpDefADJUSTR
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER(LEN(ADJUSTL(" ")))          :: I1=LEN(ADJUSTL(" "))
  INTEGER(LEN(ADJUSTL("  ")))         :: I2=LEN(ADJUSTL("  "))
  INTEGER(LEN(ADJUSTL("    ")))       :: I4=LEN(ADJUSTL("    "))
  INTEGER(LEN(ADJUSTL("12345678")))   :: I8=LEN(ADJUSTL("12345678"))

  REAL(LEN(ADJUSTL(" 12 ")))             :: R4=LEN(ADJUSTL(" 12 "))
  REAL(LEN(ADJUSTL("1234"//"5678")))     :: R8=LEN(ADJUSTL("12345678"))
  REAL(LEN(ADJUSTL(" 123456789012345"))) :: R16=LEN(ADJUSTL("1234567890123456"))

  LOGICAL(LEN(ADJUSTL(" ")))          :: L1=LEN(ADJUSTL(" ")) == 1
  LOGICAL(LEN(ADJUSTL("  ")))         :: L2=LEN(ADJUSTL("  ")) == 2
  LOGICAL(LEN(ADJUSTL("    ")))       :: L4=LEN(ADJUSTL("    ")) == 4
  LOGICAL(LEN(ADJUSTL("12345678")))   :: L8=LEN(ADJUSTL("12345678")) == 8

  COMPLEX(LEN(ADJUSTL(" 12 ")))              :: C4=(LEN(ADJUSTL("")),  &
                                               -LEN(ADJUSTL("")))
  COMPLEX(LEN(ADJUSTL("1234"//"5678")))      :: C8=(LEN(ADJUSTL(" 123456 ")), &
                                               -LEN(ADJUSTL(" 123456 ")))
  COMPLEX(LEN(ADJUSTL(" 123456789012345")))  :: C16=(LEN(ADJUSTL("1234567890123456")), &
                                                    -LEN(ADJUSTL("1234567890123456")))

  IF (KIND(I1)  .NE. 1   )                                 ERROR STOP 11
  IF (I1        .NE. 1   )                                 ERROR STOP 12

  IF (KIND(I2)  .NE. 2   )                                 ERROR STOP 13
  IF (I2        .NE. 2   )                                 ERROR STOP 14

  IF (KIND(I4)  .NE. 4   )                                 ERROR STOP 15
  IF (I4        .NE. 4   )                                 ERROR STOP 16

  IF (KIND(I8)  .NE. 8   )                                 ERROR STOP 17
  IF (I8        .NE. 8   )                                 ERROR STOP 18

  IF (KIND(R4)  .NE. 4   )                                 ERROR STOP 21
  IF (R4        .NE. 4.0 )                                 ERROR STOP 22

  IF (KIND(R8)  .NE. 8   )                                 ERROR STOP 23
  IF (R8        .NE. 8.0 )                                 ERROR STOP 24

  IF (KIND(R16) .NE. 16  )                                 ERROR STOP 25
  IF (R16       .NE. 16.0)                                 ERROR STOP 26

  IF (KIND(L1)  .NE.   1   )                               ERROR STOP 31
  IF (L1        .NEQV. .TRUE. )                            ERROR STOP 32

  IF (KIND(L2)  .NE.   2   )                               ERROR STOP 33
  IF (L2        .NEQV. .TRUE. )                            ERROR STOP 34

  IF (KIND(L4)  .NE.   4   )                               ERROR STOP 35
  IF (L4        .NEQV. .TRUE. )                            ERROR STOP 36

  IF (KIND(L8)  .NE.   8   )                               ERROR STOP 37
  IF (L8        .NEQV. .TRUE. )                            ERROR STOP 38

  IF (KIND(C4)  .NE. 4   )                                 ERROR STOP 41
  IF (C4        .NE. (0.0, -0.0) )                         ERROR STOP 42

  IF (KIND(C8)  .NE. 8   )                                 ERROR STOP 43
  IF (C8        .NE. (8.0, -8.0) )                         ERROR STOP 44

  IF (KIND(C16) .NE. 16  )                                 ERROR STOP 45
  IF (C16       .NE. (16.0, -16.0))                        ERROR STOP 46

  END


