!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefParentheses.f
!*
!*  DATE                       : Apr. 06, 2006
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
!*  An initialization expression enclosed in parentheses
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


  PROGRAM InitExpDefParentheses
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER(8),  PARAMETER :: I8(-2147483648:-2147483646) = ((((((((((-1))))))))))
  LOGICAL(1),  PARAMETER :: L1(-128:-126) = (((((((((((/.TRUE., .TRUE.,.TRUE./)))))))))))
  REAL(16),    PARAMETER :: R6(125:127) = (((((((((((/(-1.0 , I=1,3)/)))))))))))
  COMPLEX(4),  PARAMETER :: Z4(-32768:-32766) = (((((((((((1.0, -1.0)))))))))))
  TYPE(DT),    PARAMETER :: D(2147483645:2147483647)=((((((((((DT()))))))))))

  INTEGER,     PARAMETER :: SIZ=3
  INTEGER,     PARAMETER :: Con(3)=3


  INTEGER(((((((((((KIND(SIZE((/(I,I=KIND(L1), SIZE(I8))/))))))))))))))   ::  &
              TI1(((((((((((SIZE((/(I,I=KIND(L1), SIZE(I8))/)))))))))))))  =  &
                  ((((((((((SIZE((/(I,I=KIND(L1), SIZE(I8))/))))))))))))
  INTEGER(KIND(((((((((((INT(SIZE((/(I,I=KIND(L1), SIZE(L1))/)),  KIND=2)))))))))) ))) ::  &
              TI2(SIZE((((((((((((/(I,I=KIND(L1), SIZE(L1))/))))))))))))) =         &
                  SIZE((((((((((((/(I,I=KIND(L1), SIZE(L1))/))))))))))))
  INTEGER(KIND(INT(SIZE((/(I,I=KIND(((((((((((L1))))))))))), SIZE(R6))/)),  KIND=4))) ::  &
                   TI3(SIZE((/(I,I=KIND(((((((((((L1))))))))))), SIZE(R6))/))) =    &
                       SIZE((/(I,I=KIND(((((((((((L1))))))))))), SIZE(R6))/))
  INTEGER(KIND(INT(SIZE((/(I,I=KIND(L1), SIZE(D ))/)), KIND=((((((((((8))))))))))))) ::  &
              TI4(SIZE((/(I,I=KIND(L1), SIZE(((((((((((D))))))))))))/))) =  &
                          SIZE((/(I,I=KIND(L1), SIZE(((((((((((D))))))))))))/))


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



