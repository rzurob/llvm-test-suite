! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/initExp/Def/InitExpDefImpDo.f
! opt variations: -qnok -qnol

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefImpDo.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 06, 2006
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
!*  An implied-do variable within an array constructor in which each element
!*  and implied-do control expression is an initialization expression
!* 
!* 
!*  ()
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


  PROGRAM InitExpDefImpDo 
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER(8),     PARAMETER :: I8(-2147483648:-2147483646) = -1 
  LOGICAL(1),     PARAMETER :: L1(-128:-126) = .TRUE. 
  REAL(16),       PARAMETER :: R6(125:127) = -1.0 
  COMPLEX(4),     PARAMETER :: Z4(-32768:-32766) = (1.0, -1.0)
  TYPE(DT(4,20)), PARAMETER :: D(2147483645:2147483647)=DT(4,20)()
 
  INTEGER,        PARAMETER :: SIZ=3
  INTEGER,        PARAMETER :: Con(3)=(/1,2,3/) 

 
  INTEGER(KIND(SIZE((/(I,I=KIND(L1), SIZE(I8))/))))         :: TI1(SIZE((/(I,I=KIND(L1), SIZE(I8))/))) =  &
                                                                        (/(I,I=KIND(L1), SIZE(I8))/)
  INTEGER(KIND(SIZE((/(I,I=KIND(L1), SIZE(L1))/), KIND=2))) :: TI2(SIZE((/(I,I=KIND(L1), SIZE(L1))/))) =  &
                                                                        (/(I,I=KIND(L1), SIZE(L1))/)
  INTEGER(KIND(SIZE((/(I,I=KIND(L1), SIZE(R6))/), KIND=4))) :: TI3(SIZE((/(I,I=KIND(L1), SIZE(R6))/))) =  &
                                                                        (/(I,I=KIND(L1), SIZE(R6))/)
  INTEGER(KIND(SIZE((/(I,I=KIND(L1), SIZE(D ))/), KIND=8))) :: TI4(SIZE((/(I,I=KIND(L1), SIZE(D))/))) =  &
                                                                        (/(I,I=KIND(L1), SIZE(D))/)

  IF ( KIND(TI1 )  .NE. 4 )         STOP 11
  IF ( SIZE(TI1)   .NE. SIZ )       STOP 12
  IF ( ANY(TI1     .NE. Con ))      STOP 13

  IF ( KIND(TI2 )  .NE. 2 )         STOP 21
  IF ( SIZE(TI2)   .NE. SIZ )       STOP 22
  IF ( ANY(TI2     .NE. Con ))      STOP 23

  IF ( KIND(TI3 )  .NE. 4 )         STOP 31
  IF ( SIZE(TI3)   .NE. SIZ )       STOP 32
  IF ( ANY(TI3     .NE. Con ))      STOP 33

  IF ( KIND(TI4 )  .NE. 8 )         STOP 41
  IF ( SIZE(TI4)   .NE. SIZ )       STOP 42
  IF ( ANY(TI4     .NE. Con ))      STOP 43


  END


 
