!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : acesyntc497dl
!*
!*                               by David Forster)
!*  DATE                       : 2007-11-16 (original: 2006-11-19)
!*  ORIGIN                     : Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*                               (+ Array Constructor Enhancements
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement ac-do-variables
!*                               in nested implied-do's should not be repeated
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ac-implied-do, ac-do-variable
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  Test ac-implied-do's with repetition of variables.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acesyntc497dmod

  implicit none
  type derived (lderived_1) ! lderived_1=1
     integer, len :: lderived_1
  end type derived

end module acesyntc497dmod


program acesyntc497dl

  use acesyntc497dmod
  implicit none

  type (derived(1)):: darr(4) ! tcx: (1)
  logical::        larr(4)
  character::      carr(4)
  complex::        zarr(4)
  integer::        iarr(4)
  real::           rarr(4)

  integer :: i, j, k
  integer :: stmtfunOkay, stmtfunBad

  stmtfunOkay(k) = kind([integer:: ((i,i=1,k),j=1,k)])  ! Okay
  stmtfunBad(k) =  kind([integer:: ((i,i=1,k),i=1,k)])  ! Bad
  ! These are all okay:
  print *, [derived(1)::   (derived(1)(), i=1,2)] ! tcx: (1) ! tcx: (1)
  print *, [derived(1)::   ((derived(1)(), i=1,2), j=1,2)] ! tcx: (1) ! tcx: (1)
  print *, [logical::   ((i==j, i=1,2), j=1,2)]
  print *, [character:: ((char(i), i=1,2), j=1,2)]
  print *, [complex::   ((cmplx(i,i), i=1,2), j=1,2)]
  print *, [integer::   ((i, i=1,2), j=1,2)]
  print *, [real::      ((real(i), i=1,2), j=1,2)]

  darr(1:2) = [derived(1)::   (derived(1)(), i=1,2)] ! tcx: (1) ! tcx: (1)
  darr = [derived(1)::   ((derived(1)(), i=1,2), j=1,2)] ! tcx: (1) ! tcx: (1)
  larr = [logical::   ((i==j, i=1,2), j=1,2)]
  carr = [character:: ((char(i), i=1,2), j=1,2)]
  zarr = [complex::   ((cmplx(i,i), i=1,2), j=1,2)]
  iarr = [integer::   ((i, i=1,2), j=1,2)]
  rarr = [real::      ((real(i), i=1,2), j=1,2)]

  do i=1,2
     print *, [real:: ((real(i), i=1,2), j=1,1)]
  end do

  print *, [real:: [((real(i), i=1,2), j=1,1)], [((real(i), i=1,2), j=1,1)]]
  print *, [real:: [((real(i), i=1,2), j=1,1), ((real(i), i=1,2), j=1,1)]]

  print *, [integer:: (stmtfunOkay(i),i=1,1)]

  call test ([integer:: (i,i=1,1)], [integer:: (i,i=1,1)])
  call test ([integer:: (stmtfunOkay(i),i=1,1)], [integer::])

  ! Bad from here on down:
  print *, [derived(1):: ((derived(1)(),  i=1,2), i=1,2)] ! tcx: (1) ! tcx: (1)
  print *, [derived(1):: (((derived(1)(), i=1,2), i=1,2), j=1,2)] ! tcx: (1) ! tcx: (1)
  print *, [derived(1):: (((derived(1)(), i=1,2), j=1,2), i=1,2)] ! tcx: (1) ! tcx: (1)
  print *, [derived(1):: (((derived(1)(), j=1,2), i=1,2), i=1,2)] ! tcx: (1) ! tcx: (1)

  print *, [real::      (((real(i), j=1,2), i=1,2), i=1,2)]
  print *, [integer::   (((j, j=1,2), i=1,2), i=1,2)]
  print *, [complex::   (((cmplx(j,j), j=1,2), i=1,2), i=1,2)]
  print *, [character:: (((char(i), j=1,2), i=1,2), i=1,2)]
  print *, [logical::   (((logical(i==j), j=1,2), i=1,2), i=1,2)]

  darr = [derived(1)::   (((derived(1)(), i=1,2), j=1,2), i=1,1)] ! tcx: (1) ! tcx: (1)
  larr = [logical::   (((i==j, i=1,2), j=1,2), i=1,1)]
  carr = [character:: (((char(i), i=1,2), j=1,2), i=1,1)]
  zarr = [complex::   (((cmplx(i,i), i=1,2), j=1,2), i=1,1)]
  iarr = [integer::   (((i, i=1,2), j=1,2), i=1,1)]
  rarr = [real::      (((real(i), i=1,2), j=1,2), i=1,1)]

  print *, [real:: ([((real(i), i=1,2), j=1,1)], i=1,2)]
  call test ([integer:: ((i,i=1,1),i=1,1)], [integer::])
  call test ([integer::], [integer:: ((i,i=1,1),i=1,1)])

contains

  subroutine test(arg1,arg2)
    integer :: arg1(:),arg2(:), i
    print *, [integer:: (arg1(i), i=1,1)], [integer:: (arg2(i), i=1,1)]
  end subroutine test

end program acesyntc497dl


! Extensions to introduce derived type parameters:
! type: derived - added parameters (lderived_1) to invoke with (1)/declare with (*) - 19 changes
