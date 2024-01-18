!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : forall_typespec_d04.f
!*
!*  DATE                       : 2012-06-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : FORALL with type specifier (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED : implicit none
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION
!*
!*    With implicit none, the index variable must be declared.  The declaration
!*    can be in the header with a type spec, or outside the loop.  Verify that
!*    there is an error message with an undeclared index and no error message
!*    with a declared index.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

implicit none
integer(2):: k

!expect an error message here
forall(i=0:10:1)
end forall

!no errors below:
forall(integer(1)::j=0:11:2)
end forall

forall(k=0:12:3)
end forall

!expect errors here:
print*,"i=",i
print*,"j=",j
end
