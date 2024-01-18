!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : forall_typespec_f12.f
!*
!*  DATE                       : 2012-06-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : FORALL with type specifier (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED : Nesting in SE construct
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION
!*
!*    SE speculative do with forall construct/statement nested.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

integer :: i = 10
integer :: j = 10
integer :: res(3,50) = 0

!SEP$ speculative do
do i = 1,3
  forall(integer :: j = 1:50:1)
    res(i,j)= i*j
  end forall
end do
!SEP$ end speculative do

if (j .ne. 10) then
  print *, "forall incrementer modified an external scope variable, j=", j
  error stop 77
end if

if (res(3,50).ne. 150) then
  print *, "omp do { forall } yields wrong result"
  error stop 88
end if

print *, res

do i = 1,3
  forall(integer :: j = 1:50:1) res(i,j) = i*j*2
end do

if (j .ne. 10) then
  print *, "forall incrementer modified an external scope variable, j=", j
  error stop 22
end if

if (res(3,50).ne. 300) then
  print *, "do { forall } yields wrong result"
  error stop 33
endif

print *, res

end
