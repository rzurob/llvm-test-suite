!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : forall_typespec_f02.f
!*
!*  PROGRAMMER                 : Bernard Kan
!*  DATE                       : 2012-06-25
!*  ORIGIN                     : 
!*
!*  PRIMARY FUNCTIONS TESTED   : FORALL with type specifier (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED : 
!*  ADAPTED FROM               : 
!*
!*  DESCRIPTION
!*
!*    Nest forall in forall with reused variable names in the type specifier and
!*    verify the result of the computation.  Forall should not alter external 
!*    variable values
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

integer :: i = 10
integer :: sum = 0

forall(integer :: i = 1:2:1)
  forall(integer :: j = 1:50:1)
    sum = i*j
  end forall
end forall

if (i .ne. 10) then
  print *, "forall incrementer modified an external scope variable"
end if

if (sum .ne. 100) then
  print *, "forall wrong result"
end if

print *, sum
end
