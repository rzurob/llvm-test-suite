!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : autoobj83
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 31, 2009
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DTPARAM: Automatic objects
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 333321
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*
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

    IF (t%n              .NE. 4)   STOP 11
    IF (UBOUND(t%arr, 1) .NE. 4)   STOP 12
    IF (SIZE(t%arr)      .NE. 4)   STOP 13

    IF (t%arr%m               .NE. 3)   STOP 14
    IF (UBOUND(t%arr(1)%i, 1) .NE. 4)   STOP 15
    IF (SIZE(t%arr(1)%i)      .NE. 4)   STOP 16

    IF (ANY(t%arr(1)%i .NE. -1)) STOP 17

    iF (x%n              .NE. 4)   STOP 21
    IF (UBOUND(x%arr, 1) .NE. 4)   STOP 22
    IF (SIZE(x%arr)      .NE. 4)   STOP 23

    IF (x%arr%m               .NE. 3)   STOP 24
    IF (UBOUND(x%arr(1)%i, 1) .NE. 4)   STOP 25
    IF (SIZE(x%arr(1)%i)      .NE. 4)   STOP 26

    IF (ANY(x%arr(1)%i .NE. -1)) STOP 27

    END SUBROUTINE

    END

