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
!*  Expr for array bound spec
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


    PROGRAM autoobj83
    type base(m)
        integer, len :: m
        real(4) :: i(m+1)!=-1
    end type

    type dt(n)
        integer, len :: n
        type(base(n-1)) :: arr(n)
    end type

    CALL sub(4)
    CONTAINS

    SUBROUTINE Sub(N)

    type(dt(N)), pointer :: x
    type(dt(N)), target  :: t

    t%arr(1)%i = -1

    x=>t

    IF (t%n              .NE. 4)   ERROR STOP 11
    IF (UBOUND(t%arr, 1) .NE. 4)   ERROR STOP 12
    IF (SIZE(t%arr)      .NE. 4)   ERROR STOP 13

    IF (t%arr%m               .NE. 3)   ERROR STOP 14
    IF (UBOUND(t%arr(1)%i, 1) .NE. 4)   ERROR STOP 15
    IF (SIZE(t%arr(1)%i)      .NE. 4)   ERROR STOP 16

    IF (ANY(t%arr(1)%i .NE. -1)) ERROR STOP 17

    iF (x%n              .NE. 4)   ERROR STOP 21
    IF (UBOUND(x%arr, 1) .NE. 4)   ERROR STOP 22
    IF (SIZE(x%arr)      .NE. 4)   ERROR STOP 23

    IF (x%arr%m               .NE. 3)   ERROR STOP 24
    IF (UBOUND(x%arr(1)%i, 1) .NE. 4)   ERROR STOP 25
    IF (SIZE(x%arr(1)%i)      .NE. 4)   ERROR STOP 26

    IF (ANY(x%arr(1)%i .NE. -1)) ERROR STOP 27

    END SUBROUTINE

    END
