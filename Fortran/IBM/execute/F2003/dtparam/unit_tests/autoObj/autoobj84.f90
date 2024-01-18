!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : autoobj84
!*
!*  DATE                       : Jan. 31, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : DTPARAM: Automatic objects
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 333321
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  pointer/charater components
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM autoobj84

  TYPE :: Base  (l)
    INTEGER, LEN :: l
    CHARACTER(l) :: c = 'B'
  END TYPE

  TYPE, EXTENDS(Base) :: DT
    CLASS(Base(l)), POINTER :: P
  END TYPE

  CALL sub(4)
  contains

  SUBROUTINE Sub(N)


  TYPE(DT(N)),    TARGET  :: T
  CLASS(Base(N)), POINTER :: p

  allocate(t%p)
  p => T
  SELECT TYPE (T => P )
    CLASS IS (DT(*))

    IF (t%l              .NE. 4)    STOP 11

    IF (t%P%l            .NE. 4)    STOP 12
    IF (LEN(t%P%c)       .NE. 4)    STOP 13
    IF (TRIM(t%p%c)      .NE. 'B')  STOP 14

    IF (t%Base%l         .NE. 4)    STOP 22
    IF (LEN(t%Base%c)    .NE. 4)    STOP 23
    IF (TRIM(t%Base%c)   .NE. 'B')  STOP 24

  CLASS DEFAULT
    STOP 55
  END SELECT

  END SUBROUTINE

  end

