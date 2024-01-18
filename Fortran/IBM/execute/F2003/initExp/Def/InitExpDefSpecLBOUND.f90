!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefSpecLBOUND.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar 30, 2006
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
!*  - LBOUND
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


  PROGRAM  InitExpDefSpecLBOUND 
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER(1),  PARAMETER :: I1(-2147483648:-2147483647, 2147483646:2147483647) = -1 
  LOGICAL(2),  PARAMETER :: L2(-128:-127, 126:127) = .TRUE. 
  REAL(4),     PARAMETER :: R4(-128:-127, 126:127) = -1.0 
  COMPLEX(8),  PARAMETER :: Z8(-32768:-32767, 32766:32767) = (1.0, -1.0)
  TYPE(DT),    PARAMETER :: D(-2147483648:-2147483647, 2147483646:2147483647)=DT()
 
 
  INTEGER(1), PARAMETER :: LB1(2)=(/-128,  126/)
  INTEGER(1), PARAMETER :: UB1(2)=(/-127,  127/)
  INTEGER(2), PARAMETER :: LB2(2)=(/-32768, 32766/)
  INTEGER(2), PARAMETER :: UB2(2)=(/-32767, 32767/)
  INTEGER(4), PARAMETER :: LB4(2)=(/-2147483648, 2147483646/)
  INTEGER(4), PARAMETER :: UB4(2)=(/-2147483647, 2147483647/)
  INTEGER(8), PARAMETER :: LB8(2)=(/-2147483648, 2147483646/)
  INTEGER(8), PARAMETER :: UB8(2)=(/-2147483647, 2147483647/)


  INTEGER(KIND(LBOUND(I1))),          PARAMETER :: TI11(2)= LBOUND(I1) 
  INTEGER(KIND(LBOUND(I1, KIND=2))),  PARAMETER :: TI12(2)= LBOUND(L2, KIND=2) 
  INTEGER(KIND(LBOUND(I1, KIND=4))),  PARAMETER :: TI14(2)= LBOUND(I1, KIND=4) 
  INTEGER(KIND(LBOUND(I1, KIND=8))),  PARAMETER :: TI18(2)= LBOUND(I1, KIND=8) 
 
  INTEGER(KIND(LBOUND(L2))),          PARAMETER :: TL22(2)= LBOUND(L2) 
  INTEGER(KIND(LBOUND(L2, KIND=1))),  PARAMETER :: TL21(2)= LBOUND(L2, KIND=1) 
  INTEGER(KIND(LBOUND(L2, KIND=4))),  PARAMETER :: TL24(2)= LBOUND(L2, KIND=4) 
  INTEGER(KIND(LBOUND(L2, KIND=8))),  PARAMETER :: TL28(2)= LBOUND(L2, KIND=8) 
 
  INTEGER(KIND(LBOUND(R4))),          PARAMETER :: TR44(2)= UBOUND(R4) 
  INTEGER(KIND(LBOUND(R4, KIND=8))),  PARAMETER :: TR48(2)= UBOUND(R4, KIND=8) 
  INTEGER(KIND(LBOUND(R4, KIND=1))),  PARAMETER :: TR46(2)= UBOUND(R4, KIND=1) 
 
  INTEGER(KIND(LBOUND(Z8))),          PARAMETER :: TZ88(2)= UBOUND(Z8) 
  INTEGER(KIND(LBOUND(Z8, KIND=4))),  PARAMETER :: TZ84(2)= UBOUND(Z8, KIND=4) 
  INTEGER(KIND(LBOUND(Z8, KIND=2))),  PARAMETER :: TZ86(2)= UBOUND(Z8, KIND=2) 
 
  INTEGER,                            PARAMETER :: TDL(2) = LBOUND(D) 
  INTEGER,                            PARAMETER :: TDU(2) = UBOUND(D) 



  IF ( KIND(TI11 )   .NE. 4 )             STOP 11
  IF ( ANY( TI11     .NE. LB4 ))          STOP 12
  IF ( KIND(TI12 )   .NE. 2 )             STOP 13
  IF ( ANY( TI12     .NE. LB1 ))          STOP 14
  IF ( KIND(TI14 )   .NE. 4 )             STOP 15
  IF ( ANY( TI14     .NE. LB4 ))          STOP 16
  IF ( KIND(TI18 )   .NE. 8 )             STOP 17
  IF ( ANY( TI18     .NE. LB4 ))          STOP 18

  IF ( KIND(TL21 )   .NE. 1 )             STOP 21
  IF ( ANY( TL21     .NE. LB1 ))          STOP 22
  IF ( KIND(TL22 )   .NE. 4 )             STOP 23
  IF ( ANY( TL22     .NE. LB1 ))          STOP 24
  IF ( KIND(TL24 )   .NE. 4 )             STOP 25
  IF ( ANY( TL24     .NE. LB1 ))          STOP 26
  IF ( KIND(TL28 )   .NE. 8 )             STOP 27
  IF ( ANY( TL28     .NE. LB1 ))          STOP 28

  IF ( KIND(TR44 )   .NE. 4 )             STOP 31
  IF ( ANY( TR44     .NE. UB1 ))          STOP 32
  IF ( KIND(TR48 )   .NE. 8 )             STOP 33
  IF ( ANY( TR48     .NE. UB1 ))          STOP 34
  IF ( KIND(TR46 )   .NE. 1  )            STOP 35
  IF ( ANY( TR46     .NE. UB1 ))          STOP 36

  IF ( KIND(TZ88 )   .NE. 4 )             STOP 41
  IF ( ANY( TZ88     .NE. UB2 ))          STOP 42
  IF ( KIND(TZ84 )   .NE. 4 )             STOP 43
  IF ( ANY( TZ84     .NE. UB2 ))          STOP 44
  IF ( KIND(TZ86 )   .NE. 2  )            STOP 45
  IF ( ANY( TZ86     .NE. UB2 ))          STOP 46

  IF ( ANY( TDL      .NE. LB8 ))          STOP 51
  IF ( ANY( TDU      .NE. UB8 ))          STOP 52

  END


 
