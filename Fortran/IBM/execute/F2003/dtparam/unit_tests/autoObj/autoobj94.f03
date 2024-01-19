!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 31, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : DTPARAM: Automatic objects
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 333321
!*********************************************************************
!*  ===================================================================
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
!*  Multi levels of type -- array dummy
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM autoobj03

  TYPE dt0(l)
    INTEGER, LEN :: l
    integer :: arr(l)
  END TYPE

  TYPE dt1(l)
    INTEGER, LEN :: l
    TYPE(dt0(l)) :: arr(l)!=dt0(4)(-1)
  END TYPE

  TYPE dt2(l)
    INTEGER, LEN :: l
    TYPE(dt1(l)) :: arr(1:l)
  END TYPE

  CALL Sub(4)

  CONTAINS

  SUBROUTINE Sub(N)

  TYPE(dt2(n)) t(N)

    t(1)%arr(1)%arr = dt0(4)(-1)

    IF (t%l                   .NE. 4)   ERROR STOP 11
    IF (t(1)%arr%l            .NE. 4)   ERROR STOP 12
    IF (t(1)%arr(1)%arr%l     .NE. 4)   ERROR STOP 13
    IF (t(1)%arr(1)%arr(1)%l  .NE. 4)   ERROR STOP 14

    IF (UBOUND(t, 1)                      .NE. 4)   ERROR STOP 21
    IF (UBOUND(t(1)%arr, 1)               .NE. 4)   ERROR STOP 22
    IF (UBOUND(t(1)%arr(1)%arr, 1)        .NE. 4)   ERROR STOP 23
    IF (UBOUND(t(1)%arr(1)%arr(1)%arr, 1) .NE. 4)   ERROR STOP 24

    IF (SIZE(t)                           .NE. 4)   ERROR STOP 31
    IF (SIZE(t(1)%arr)                    .NE. 4)   ERROR STOP 32
    IF (SIZE(t(1)%arr(1)%arr)             .NE. 4)   ERROR STOP 33
    IF (SIZE(t(1)%arr(1)%arr(1)%arr)      .NE. 4)   ERROR STOP 34

    IF (ANY(t(1)%arr(1)%arr(1)%arr        .NE. -1) )   ERROR STOP 41
    IF (ANY(t(1)%arr(1)%arr%arr(1)        .NE. -1) )   ERROR STOP 42

    t(1)%arr%arr(1) = dt0(4)(-1)
    IF (ANY(t(1)%arr%arr(1)%arr(1)        .NE. -1) )   ERROR STOP 43

    t%arr(1)%arr(1) = dt0(4)(-1)
    IF (ANY(t%arr(1)%arr(1)%arr(1)        .NE. -1) )   ERROR STOP 44

  END SUBROUTINE

  END

