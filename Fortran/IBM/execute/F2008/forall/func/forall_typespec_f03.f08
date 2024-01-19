!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2012-06-25
!*
!*  PRIMARY FUNCTIONS TESTED   : FORALL with type specifier (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED : nesting in OMP parallel do
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION
!*
!*    Nest forall in an omp parallel do loop and verify the result of the
!*    computation.  Forall should not alter external variable values.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

integer :: i = 10
integer :: j = 10
integer :: res(3,50) = 0

!$omp parallel do
do i = 1,3
  forall(integer :: j = 1:50:1)
    res(i,j) = i*j
  end forall
end do
!$omp end parallel do

if (j .ne. 10) then
  print *, "forall incrementer modified an external scope variable, j=", j
  error stop 66
end if

if (res(3,50) .ne. 150) then
  print *, "omp do { forall } yields wrong result"
  error stop 77
end if

print *, res

end
