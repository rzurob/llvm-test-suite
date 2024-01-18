!#######################################################################
!************************************************************************
! %START
! %MAIN: YES
! %PRECMD:  $TR_SRC/run.sh fxmdve14 cxmdve14
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!************************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxmdve14.f
!*
!*  DATE                       : Sep 14,2002
!*
!*  PRIMARY FUNCTIONS TESTED   : Test 1  dimension integer array variables,
!*                               the combination of type and kind type
!*                               parameter ,with bind(c) attribute/statement,
!*                               is interoperate with  corresponding C type.
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DEPENDENCIES               : External routine ZZRC
!*
!*  KEYWORD(S)                 : C_INT_LEAST32_T , DIMENSION
!*
!*  DESCRIPTION                :
!*                              Pass data between a C array with external
!*				linkage and Fortran array has the bind(c)
!*				attribute.
!*                              Verify the result of data passing.
!*
!*                              Define 1 dimension array variable in
!*                              the module mod and the varibles are
!*                              accessed by other fortran  program units and
!*                              c code.
!*                              The array sorting  SUBROUTINE REARRANGE is
!*                              encapsulated in the module named
!*                              MODULE ORDER_AN_ARRAY, and is called in
!*                              Fortran Main  PROGRAM RANDOM_ARRAY_ORDERED.
!*                              and the subroutine  REARRANGE initialize
!*                              the varibles, then call C function to check
!*                              if the value is passed to C code correctly,
!*                              then modify the value of the variables in C
!*                              code and passed back to Fortran surbroutine
!*                              REARRANGE, then sort the  array  and
!*                              pass back the value to Fortran main program
!*                              to check the correctness.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  09/14/03    KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mod
  USE ISO_C_BINDING
  integer(C_INT_LEAST32_T), DIMENSION(5),bind(c) :: A


CONTAINS
  function assert_eq2(n1,n2)
    INTEGER(C_INT_LEAST32_T), INTENT(IN) :: n1,n2
    LOGICAL assert_eq2
    if (n1 .NE. n2) then
       assert_eq2= .FALSE.
    else
       assert_eq2=.TRUE.
    endif
    return
  END function assert_eq2
end module mod

!
MODULE ORDER_AN_ARRAY
  PRIVATE EXCHANGE
  !
CONTAINS
  !
  SUBROUTINE REARRANGE
    USE MOD
    LOGICAL, ALLOCATABLE :: MASK(:)
    INTEGER :: I, N
    INTEGER, DIMENSION(1):: K
    A=(/-20,8,1,5,10/)
    print *,A
    ! Call C function
    call csub()

    print *, A
    N = SIZE (A)
    ALLOCATE (MASK(N))
    MASK = .TRUE.

    mainloop: DO I = 0, N-1

       MASK(N-I) = .FALSE.
       K  = MAXLOC(A,MASK)

       IF (K(1)==0) EXIT mainloop

       Do while  (A(K(1)) > A(N-I))
          CALL EXCHANGE(A(K(1)),A(N-I))
       enddo

    END DO mainloop

  END SUBROUTINE REARRANGE


  !
  SUBROUTINE EXCHANGE (X,Y)
    USE ISO_C_BINDING
    integer(C_INT_LEAST32_T):: X,Y
    integer(C_INT_LEAST32_T):: TX
    TX = X; X = Y; Y = TX
  END SUBROUTINE EXCHANGE
  !
END MODULE ORDER_AN_ARRAY


!
PROGRAM RANDOM_ARRAY_ORDERED
  USE MOD
  USE ORDER_AN_ARRAY
  LOGICAL ::result
  integer(C_INT_LEAST32_T)::basval1,basval2
  basval1 = -100
  basval2 = 500
  !
  CALL REARRANGE
  result= assert_eq2(A(1),basval1) .AND.assert_eq2(A(5),basval2)
  if (result .eqv. .FALSE.) then
     error stop 230
  endif

  print *, A
END PROGRAM RANDOM_ARRAY_ORDERED

