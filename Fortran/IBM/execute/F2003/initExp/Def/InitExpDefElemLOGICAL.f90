!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 12, 2006
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
!*  -  LOGICAL
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM  InitExpDefElemLOGICAL
  !IMPLICIT LOGICAL(KIND(LOGICAL(L)))(L)
  INTEGER :: I, J

  LOGICAL, PARAMETER :: L (128)=.TRUE.
  LOGICAL, PARAMETER :: L1(128)=LOGICAL( L .AND. .NOT. L)


  TYPE :: DT0
    LOGICAL(KIND(LOGICAL(L=L, KIND=1))) :: L1(SIZE(L))=LOGICAL(L=L, KIND=1)
    LOGICAL(KIND(LOGICAL(L=L, KIND=2))) :: L2(SIZE(L))=LOGICAL(L=L, KIND=2)
    LOGICAL(KIND(LOGICAL(L=L, KIND=4))) :: L4(SIZE(L))=LOGICAL(L=L, KIND=4)
    LOGICAL(KIND(LOGICAL(L=L, KIND=8))) :: L8(SIZE(L))=LOGICAL(L=L, KIND=8)
  END TYPE

  TYPE (DT0) :: T
  TYPE (DT0) :: T1=DT0(                                    &
                        L8=LOGICAL(L=L1, KIND=8),          &
                        L4=LOGICAL(L=L1, KIND=4),          &
                        L2=LOGICAL(L=L1, KIND=2),          &
                        L1=LOGICAL(L=L1, KIND=1)           &
                      )

  TYPE (DT0) :: T2=DT0(                                    &
                        L8=ANY((/LOGICAL(L=L1, KIND=8) .NEQV. L/)),          &
                        L4=ANY((/LOGICAL(L=L1, KIND=4) .NEQV. L/)),          &
                        L2=ANY((/LOGICAL(L=L1, KIND=2) .NEQV. L/)),          &
                        L1=ANY((/LOGICAL(L=L1, KIND=1) .NEQV. L/))           &
                      )


  IF ( KIND(T%L1)  .NE.   1 )      STOP 11
  IF ( ANY (T%L1)  .NEQV. .TRUE. ) STOP 12
  IF ( KIND(T%L2)  .NE.   2 )      STOP 13
  IF ( ANY (T%L2)  .NEQV. .TRUE. ) STOP 14
  IF ( KIND(T%L4)  .NE.   4 )      STOP 15
  IF ( ANY (T%L4)  .NEQV. .TRUE. ) STOP 16
  IF ( KIND(T%L8)  .NE.   8 )      STOP 17
  IF ( ANY (T%L8)  .NEQV. .TRUE. ) STOP 18

  IF ( ANY (T1%L1)  .NEQV. .FALSE. ) STOP 22
  IF ( ANY (T1%L2)  .NEQV. .FALSE. ) STOP 24
  IF ( ANY (T1%L4)  .NEQV. .FALSE. ) STOP 26
  IF ( ANY (T1%L8)  .NEQV. .FALSE. ) STOP 28

  IF ( ANY (T2%L1)  .NEQV. .TRUE. )  STOP 32
  IF ( ANY (T2%L2)  .NEQV. .TRUE. )  STOP 34
  IF ( ANY (T2%L4)  .NEQV. .TRUE. )  STOP 36
  IF ( ANY (T2%L8)  .NEQV. .TRUE. )  STOP 38

  END


