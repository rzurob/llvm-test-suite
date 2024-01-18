!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefSpecSHAPE.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 03, 2006
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
!*  a reference to a specification inquiry 
!* 
!*  - SHAPE 
!* 
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT
    PROCEDURE(CHARACTER), POINTER, NOPASS :: ProcPtr=>NULL()
    CONTAINS
    PROCEDURE  :: Proc => ModSub
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(Arg)
  CLASS(DT) :: Arg
  END SUBROUTINE

  END MODULE


  PROGRAM  InitExpDefSpecSHAPE
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER(8),  PARAMETER :: I8(-2147483648:-2147483647, 2147483646:2147483647) = -1 
  LOGICAL(1),  PARAMETER :: L2(-128:-127, 126:127) = .TRUE. 
  REAL(16),    PARAMETER :: R6(-128:-127, 126:127) = -1.0 
  COMPLEX(4),  PARAMETER :: Z4(-32768:-32767, 32766:32767) = (1.0, -1.0)
  TYPE(DT),    PARAMETER :: D(-2147483648:-2147483647, 2147483646:2147483647)=DT()
 
  INTEGER,     PARAMETER :: SHP(2)=(/2, 2/)

 
  CALL SI8(I8)
  CALL SL1(L2)
  CALL SR6(R6)
  CALL SZ4(Z4)
  CALL SD (D)

  CONTAINS

  SUBROUTINE SI8(IArr)
  INTEGER(8)                                     :: IArr(-2147483648:-2147483647, 2147483646:2147483647)
  INTEGER(KIND(SHAPE(IArr))),          PARAMETER :: TI11(2)= SHAPE(IArr) 
  INTEGER(KIND(SHAPE(IArr, KIND=2))),  PARAMETER :: TI12(2)= SHAPE(IArr, KIND=2) 
  INTEGER(KIND(SHAPE(IArr, KIND=4))),  PARAMETER :: TI14(2)= SHAPE(IArr, KIND=4) 
  INTEGER(KIND(SHAPE(IArr, KIND=8))),  PARAMETER :: TI18(2)= SHAPE(IArr, KIND=8) 
 
  IF ( KIND(TI11 )   .NE. 4 )             STOP 11
  IF ( ANY( TI11     .NE. SHP ))          STOP 12
  IF ( KIND(TI12 )   .NE. 2 )             STOP 13
  IF ( ANY( TI12     .NE. SHP ))          STOP 14
  IF ( KIND(TI14 )   .NE. 4 )             STOP 15
  IF ( ANY( TI14     .NE. SHP ))          STOP 16
  IF ( KIND(TI18 )   .NE. 8 )             STOP 17
  IF ( ANY( TI18     .NE. SHP ))          STOP 18

  END SUBROUTINE
 
  SUBROUTINE SL1(LArr)
  LOGICAL(1)                                      :: LArr(-128:-127, 126:127)
  INTEGER(KIND(SHAPE(LArr))),          PARAMETER :: TL22(2)= SHAPE(LArr) 
  INTEGER(KIND(SHAPE(LArr, KIND=1))),  PARAMETER :: TL21(2)= SHAPE(LArr, KIND=1) 
  INTEGER(KIND(SHAPE(LArr, KIND=4))),  PARAMETER :: TL24(2)= SHAPE(LArr, KIND=4) 
  INTEGER(KIND(SHAPE(LArr, KIND=8))),  PARAMETER :: TL28(2)= SHAPE(LArr, KIND=8) 
 
  IF ( KIND(TL21 )   .NE. 1 )             STOP 21
  IF ( ANY( TL21     .NE. SHP ))          STOP 22
  IF ( KIND(TL22 )   .NE. 4 )             STOP 23
  IF ( ANY( TL22     .NE. SHP ))          STOP 24
  IF ( KIND(TL24 )   .NE. 4 )             STOP 25
  IF ( ANY( TL24     .NE. SHP ))          STOP 26
  IF ( KIND(TL28 )   .NE. 8 )             STOP 27
  IF ( ANY( TL28     .NE. SHP ))          STOP 28

  END SUBROUTINE
 
  SUBROUTINE SR6(RArr)
  REAL(16)      :: RArr(-128:-127, 126:127)  
  INTEGER(KIND(SHAPE(RArr))),          PARAMETER :: TR44(2)= SHAPE(RArr) 
  INTEGER(KIND(SHAPE(RArr, KIND=8))),  PARAMETER :: TR48(2)= SHAPE(RArr, KIND=8) 
  INTEGER(KIND(SHAPE(RArr, KIND=2))), PARAMETER :: TR46(2)= SHAPE(RArr, KIND=2) 
 
  IF ( KIND(TR44 )   .NE. 4 )             STOP 31
  IF ( ANY( TR44     .NE. SHP ))          STOP 32
  IF ( KIND(TR48 )   .NE. 8 )             STOP 33
  IF ( ANY( TR48     .NE. SHP ))          STOP 34
  IF ( KIND(TR46 )   .NE. 2 )             STOP 35
  IF ( ANY( TR46     .NE. SHP ))          STOP 36

  END SUBROUTINE

  SUBROUTINE SZ4(ZArr)
  COMPLEX(4)   :: ZArr(-32768:-32767, 32766:32767)
  INTEGER(KIND(SHAPE(ZArr))),          PARAMETER :: TZ88(2)= SHAPE(ZArr) 
  INTEGER(KIND(SHAPE(ZArr, KIND=4))),  PARAMETER :: TZ84(2)= SHAPE(ZArr, KIND=4) 
  INTEGER(KIND(SHAPE(ZArr, KIND=1))),  PARAMETER :: TZ86(2)= SHAPE(ZArr, KIND=1) 
 
  IF ( KIND(TZ88 )   .NE. 4 )             STOP 41
  IF ( ANY( TZ88     .NE. SHP ))          STOP 42
  IF ( KIND(TZ84 )   .NE. 4 )             STOP 43
  IF ( ANY( TZ84     .NE. SHP ))          STOP 44
  IF ( KIND(TZ86 )   .NE. 1  )            STOP 45
  IF ( ANY( TZ86     .NE. SHP ))          STOP 46

  END SUBROUTINE

  SUBROUTINE SD(DArr)
  TYPE(DT) :: DArr(-2147483648:-2147483647, 2147483646:2147483647)
  INTEGER,         PARAMETER :: TDL(2) = SHAPE(D) 
  INTEGER,         PARAMETER :: SHP1   = SIZE(SHAPE(D(-2147483648,2147483646))) 

  IF ( ANY( TDL      .NE. SHP ))         STOP 51
  IF ( SHP1          .NE. 0 )            STOP 52

  END SUBROUTINE

  END


 
