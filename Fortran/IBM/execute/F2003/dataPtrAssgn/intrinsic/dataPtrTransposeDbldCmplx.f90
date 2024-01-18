!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrTransposeDbldCmplx.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-pointer used as arg of transpose, type double complex
!* - zero-size for dim 2
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

double complex, pointer :: bP(:,:)
double complex, target, allocatable :: aT(:)

allocate(aT(10), source=(/ (cmplx(i-1,i+1,8), i= 1,10 )/) )

bp(11:20,2:1) => aT(:10)

if ( any( lbound(bp) .ne. (/11, 1/) ) ) stop 11
if ( any( ubound(bp) .ne. (/20, 0/) ) ) stop 15
if ( any( shape(transpose(bP)) .ne. (/0, 10/))) stop 22

end program
