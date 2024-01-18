!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : forall_typespec_f13.f
!*
!*  PROGRAMMER                 : Bernard Kan
!*  DATE                       : 2012-06-25
!*  ORIGIN                     : 
!*
!*  PRIMARY FUNCTIONS TESTED   : FORALL with type specifier (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED : Nesting in TM construct
!*  ADAPTED FROM               : 
!*
!*  DESCRIPTION
!*
!*    TM atomic region with forall construct/statement nested and vice versa.  
!*    Also contains an TM nested in an SMP section with a forall construct 
!*    nested in the TM section.  
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

integer :: i = 10
integer :: j = 10
integer :: res(3,50) = 0

!TM$ tm_atomic
do i = 1,3
  forall(integer :: j = 1:50:1)
    res(i,j) = i*j
  end forall
end do
!TM$ end tm_atomic


if (j .ne. 10) then
  print *, "forall incrementer modified an external scope variable, j=", j
  error stop 77
end if

print *, res

if (res(3,50) .ne. 150) then
  print *, "omp do { forall } yields wrong result"
  error stop 88
end if

!SMP$ independent
do i = 1,3
  !TM$ tm_atomic
  forall(integer :: j = 1:50:1) res(i,j) = i*j*2
  !TM$ end tm_atomic
end do

if (j .ne. 10) then
  print *, "forall incrementer modified an external scope variable, j=", j
  error stop 22
end if

print *, res

if (res(3,50) .ne. 300) then
  print *, "do { forall } yields wrong result"
  error stop 33
endif

end
