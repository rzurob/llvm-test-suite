! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/initExp/Def/InitExpDefSpecUBOUND.f
! opt variations: -qnok -qnol

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
!*  - UBOUND
!*
!*  (325202)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(K1,N1)    ! (4,20)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    PROCEDURE(CHARACTER), POINTER, NOPASS :: ProcPtr=>NULL()
    CONTAINS
    PROCEDURE  :: Proc => ModSub
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(Arg)
  CLASS(DT(4,*)) :: Arg
  END SUBROUTINE

  END MODULE


  PROGRAM  InitExpDefSpecLBOUND
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER(1),     PARAMETER :: I1(-2147483648:-2147483647, 2147483646:2147483647) = -1
  LOGICAL(2),     PARAMETER :: L2(-128:-127, 126:127) = .TRUE.
  REAL(4),        PARAMETER :: R4(-128:-127, 126:127) = -1.0
  COMPLEX(8),     PARAMETER :: Z8(-32768:-32767, 32766:32767) = (1.0, -1.0)
  TYPE(DT(4,20)), PARAMETER :: D(-2147483648:-2147483647, 2147483646:2147483647)=DT(4,20)()

  INTEGER(1), PARAMETER :: LB1(2)=(/-128,  126/)
  INTEGER(1), PARAMETER :: UB1(2)=(/-127,  127/)
  INTEGER(2), PARAMETER :: LB2(2)=(/-32768, 32766/)
  INTEGER(2), PARAMETER :: UB2(2)=(/-32767, 32767/)
  INTEGER(4), PARAMETER :: LB4(2)=(/-2147483648, 2147483646/)
  INTEGER(4), PARAMETER :: UB4(2)=(/-2147483647, 2147483647/)
  INTEGER(8), PARAMETER :: LB8(2)=(/-2147483648, 2147483646/)
  INTEGER(8), PARAMETER :: UB8(2)=(/-2147483647, 2147483647/)


  CALL SI1(I1)
  CALL SL2(L2)
  CALL SR4(R4)
  CALL SR41(R4)
  CALL SZ8(Z8)
  CALL SD (D)

  CONTAINS

  SUBROUTINE SI1(IArr)
  INTEGER(1)                                      :: IArr(-2147483648:-2147483647, 2147483646:*)
  INTEGER(KIND(LBOUND(IArr))),          PARAMETER :: TI11(2)= LBOUND(IArr)
  INTEGER(KIND(LBOUND(IArr, KIND=2))),  PARAMETER :: TI12(2)= LBOUND(IArr, KIND=2)
  INTEGER(KIND(LBOUND(IArr, KIND=4))),  PARAMETER :: TI14(2)= LBOUND(IArr, KIND=4)
  INTEGER(KIND(LBOUND(IArr, KIND=8))),  PARAMETER :: TI18(2)= LBOUND(IArr, KIND=8)

  IF ( KIND(TI11 )   .NE. 4 )             STOP 11
! IF ( ANY( TI11     .NE. LB4 ))          STOP 12
  IF ( KIND(TI12 )   .NE. 2 )             STOP 13
! IF ( ANY( TI12     .NE. LB4 ))          STOP 14
  IF ( KIND(TI14 )   .NE. 4 )             STOP 15
  IF ( ANY( TI14     .NE. LB4 ))          STOP 16
  IF ( KIND(TI18 )   .NE. 8 )             STOP 17
  IF ( ANY( TI18     .NE. LB4 ))          STOP 18

  END SUBROUTINE

  SUBROUTINE SL2(LArr)
  LOGICAL(2)                                      :: LArr(-128:-127, 126:*)
  INTEGER(KIND(LBOUND(LArr))),          PARAMETER :: TL22(2)= LBOUND(LArr)
  INTEGER(KIND(LBOUND(LArr, KIND=1))),  PARAMETER :: TL21(2)= LBOUND(LArr, KIND=1)
  INTEGER(KIND(LBOUND(LArr, KIND=4))),  PARAMETER :: TL24(2)= LBOUND(LArr, KIND=4)
  INTEGER(KIND(LBOUND(LArr, KIND=8))),  PARAMETER :: TL28(2)= LBOUND(LArr, KIND=8)

  IF ( KIND(TL21 )   .NE. 1 )             STOP 21
  IF ( ANY( TL21     .NE. LB1 ))          STOP 22
  IF ( KIND(TL22 )   .NE. 4 )             STOP 23
  IF ( ANY( TL22     .NE. LB1 ))          STOP 24
  IF ( KIND(TL24 )   .NE. 4 )             STOP 25
  IF ( ANY( TL24     .NE. LB1 ))          STOP 26
  IF ( KIND(TL28 )   .NE. 8 )             STOP 27
  IF ( ANY( TL28     .NE. LB1 ))          STOP 28

  END SUBROUTINE

  SUBROUTINE SR4(RArr)
  REAL(4)      :: RArr(126:127, -128:-127)
  INTEGER(KIND(LBOUND(RArr))),          PARAMETER :: TR44(2)= LBOUND(RArr)
  INTEGER(KIND(LBOUND(RArr, KIND=8))),  PARAMETER :: TR48(2)= LBOUND(RArr, KIND=8)
  INTEGER(KIND(LBOUND(RArr, KIND=1 ))), PARAMETER :: TR46(2)= LBOUND(RArr, KIND=1 )

  IF ( KIND(TR44 )   .NE. 4 )             STOP 31
  IF ( ANY( TR44     .NE. [126,-128] ))   STOP 32
  IF ( KIND(TR48 )   .NE. 8 )             STOP 33
  IF ( ANY( TR48     .NE. [126,-128] ))   STOP 34
  IF ( KIND(TR46 )   .NE. 1  )            STOP 35
  IF ( ANY( TR46     .NE. [126,-128] ))   STOP 36

  END SUBROUTINE

  SUBROUTINE SR41(RArr)
  REAL(4)      :: RArr(1:, 1:)
  INTEGER(KIND(LBOUND(RArr))),          PARAMETER :: TR44(2)= LBOUND(RArr)
  INTEGER(KIND(LBOUND(RArr, KIND=8))),  PARAMETER :: TR48(2)= LBOUND(RArr, KIND=8)
  INTEGER(KIND(LBOUND(RArr, KIND=1 ))), PARAMETER :: TR46(2)= LBOUND(RArr, KIND=1 )

  IF ( KIND(TR44 )   .NE. 4 )           STOP 61
  IF ( ANY( TR44     .NE. 1 ))          STOP 62
  IF ( KIND(TR48 )   .NE. 8 )           STOP 63
  IF ( ANY( TR48     .NE. 1 ))          STOP 64
  IF ( KIND(TR46 )   .NE. 1  )          STOP 65
  IF ( ANY( TR46     .NE. 1 ))          STOP 66

  END SUBROUTINE

  SUBROUTINE SZ8(ZArr)
  COMPLEX(8)   :: ZArr(-32768:-32767, 32766:32767)
  INTEGER(KIND(LBOUND(ZArr))),          PARAMETER :: TZ88(2)= UBOUND(ZArr)
  INTEGER(KIND(LBOUND(ZArr, KIND=4))),  PARAMETER :: TZ84(2)= UBOUND(ZArr, KIND=4)
  INTEGER(KIND(LBOUND(ZArr, KIND=2 ))), PARAMETER :: TZ86(2)= UBOUND(ZArr, KIND=2 )

  IF ( KIND(TZ88 )   .NE. 4 )             STOP 41
  IF ( ANY( TZ88     .NE. UB2 ))          STOP 42
  IF ( KIND(TZ84 )   .NE. 4 )             STOP 43
  IF ( ANY( TZ84     .NE. UB2 ))          STOP 44
  IF ( KIND(TZ86 )   .NE. 2  )            STOP 45
  IF ( ANY( TZ86     .NE. UB2 ))          STOP 46

  END SUBROUTINE

  SUBROUTINE SD(DArr)
  TYPE(DT(4,*))      :: DArr(2147483646:2147483647,-2147483648:-2147483647)
  INTEGER, PARAMETER :: TDL(2) = LBOUND(DArr)

  IF ( ANY( TDL      .NE. [2147483646, -2147483648] ))  STOP 51

  END SUBROUTINE

  END



