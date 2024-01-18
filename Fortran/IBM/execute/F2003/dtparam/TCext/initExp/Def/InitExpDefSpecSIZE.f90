! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/initExp/Def/InitExpDefSpecSIZE.f
! opt variations: -qnok -ql

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 03, 2006
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
!*  - SIZE
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    PROCEDURE(CHARACTER), POINTER, NOPASS :: ProcPtr=>NULL()
    CONTAINS
    PROCEDURE  :: Proc => ModSub
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(Arg)
  CLASS(DT(4)) :: Arg
  END SUBROUTINE

  END MODULE


  PROGRAM  InitExpDefSpecSIZE
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER(8),  PARAMETER :: I8(-2147483648:-2147483647, 2147483646:2147483647) = -1
  LOGICAL(1),  PARAMETER :: L2(-128:-127, 126:127) = .TRUE.
  REAL(16),    PARAMETER :: R6(-128:-127, 126:127) = -1.0
  COMPLEX(4),  PARAMETER :: Z4(-32768:-32767, 32766:32767) = (1.0, -1.0)
  TYPE(DT(4)), PARAMETER :: D(-2147483648:-2147483647, 2147483646:2147483647)=DT(4)()

  INTEGER,     PARAMETER :: SIZ=4


  CALL SI8(I8)
  CALL SL1(L2)
  CALL SR6(R6)
  CALL SZ4(Z4)
  CALL SD (D)

  CONTAINS

  SUBROUTINE SI8(IArr)
  INTEGER(8)                                    :: IArr(-2147483648:-2147483647, 2147483646:2147483647)
  INTEGER(KIND(SIZE(IArr))),          PARAMETER :: TI11= SIZE(IArr)
  INTEGER(KIND(SIZE(IArr, KIND=2))),  PARAMETER :: TI12= SIZE(IArr, KIND=2)
  INTEGER(KIND(SIZE(IArr, KIND=4))),  PARAMETER :: TI14= SIZE(IArr, KIND=4)
  INTEGER(KIND(SIZE(IArr, KIND=8))),  PARAMETER :: TI18= SIZE(IArr, KIND=8)

  IF ( KIND(TI11 )   .NE. 4 )         ERROR STOP 11
  IF (  TI11     .NE. SIZ )           ERROR STOP 12
  IF ( KIND(TI12 )   .NE. 2 )         ERROR STOP 13
  IF (  TI12     .NE. SIZ )           ERROR STOP 14
  IF ( KIND(TI14 )   .NE. 4 )         ERROR STOP 15
  IF (  TI14     .NE. SIZ )           ERROR STOP 16
  IF ( KIND(TI18 )   .NE. 8 )         ERROR STOP 17
  IF (  TI18     .NE. SIZ )           ERROR STOP 18

  END SUBROUTINE

  SUBROUTINE SL1(LArr)
  LOGICAL(1)                                    :: LArr(-128:-127, 126:127)
  INTEGER(KIND(SIZE(LArr))),          PARAMETER :: TL22= SIZE(LArr)
  INTEGER(KIND(SIZE(LArr, KIND=1))),  PARAMETER :: TL21= SIZE(LArr, KIND=1)
  INTEGER(KIND(SIZE(LArr, KIND=4))),  PARAMETER :: TL24= SIZE(LArr, KIND=4)
  INTEGER(KIND(SIZE(LArr, KIND=8))),  PARAMETER :: TL28= SIZE(LArr, KIND=8)

  IF ( KIND(TL21 )   .NE. 1 )        ERROR STOP 21
  IF (  TL21         .NE. SIZ )      ERROR STOP 22
  IF ( KIND(TL22 )   .NE. 4 )        ERROR STOP 23
  IF (  TL22         .NE. SIZ )      ERROR STOP 24
  IF ( KIND(TL24 )   .NE. 4 )        ERROR STOP 25
  IF (  TL24         .NE. SIZ )      ERROR STOP 26
  IF ( KIND(TL28 )   .NE. 8 )        ERROR STOP 27
  IF (  TL28         .NE. SIZ )      ERROR STOP 28

  END SUBROUTINE

  SUBROUTINE SR6(RArr)
  REAL(16)      :: RArr(-128:-127, 126:127)
  INTEGER(KIND(SIZE(RArr))),          PARAMETER :: TR44= SIZE(RArr)
  INTEGER(KIND(SIZE(RArr, KIND=8))),  PARAMETER :: TR48= SIZE(RArr, KIND=8)
  INTEGER(KIND(SIZE(RArr, KIND=1 ))), PARAMETER :: TR46= SIZE(RArr, KIND=1 )

  IF ( KIND(TR44 )   .NE. 4  )       ERROR STOP 31
  IF (  TR44         .NE. SIZ )      ERROR STOP 32
  IF ( KIND(TR48 )   .NE. 8 )        ERROR STOP 33
  IF (  TR48         .NE. SIZ )      ERROR STOP 34
  IF ( KIND(TR46 )   .NE. 1  )       ERROR STOP 35
  IF (  TR46         .NE. SIZ )      ERROR STOP 36

  END SUBROUTINE

  SUBROUTINE SZ4(ZArr)
  COMPLEX(4)   :: ZArr(-32768:-32767, 32766:32767)
  INTEGER(KIND(SIZE(ZArr))),          PARAMETER :: TZ88= SIZE(ZArr)
  INTEGER(KIND(SIZE(ZArr, KIND=4))),  PARAMETER :: TZ84= SIZE(ZArr, KIND=4)
  INTEGER(KIND(SIZE(ZArr, KIND=2 ))), PARAMETER :: TZ86= SIZE(ZArr, KIND=2 )

  IF ( KIND(TZ88 )   .NE. 4 )        ERROR STOP 41
  IF (  TZ88         .NE. SIZ )      ERROR STOP 42
  IF ( KIND(TZ84 )   .NE. 4 )        ERROR STOP 43
  IF (  TZ84         .NE. SIZ )      ERROR STOP 44
  IF ( KIND(TZ86 )   .NE. 2  )       ERROR STOP 45
  IF (  TZ86         .NE. SIZ )      ERROR STOP 46

  END SUBROUTINE

  SUBROUTINE SD(DArr)
  TYPE(DT(4)) :: DArr(-2147483648:-2147483647, 2147483646:2147483647)
  INTEGER,                            PARAMETER :: TDL = SIZE(D)
  INTEGER,                            PARAMETER :: SIZ1 = SIZE(D, DIM=1)

  IF (  TDL      .NE. SIZ )          ERROR STOP 51
  IF (  SIZ1     .NE. 2 )            ERROR STOP 52

  END SUBROUTINE

  END



