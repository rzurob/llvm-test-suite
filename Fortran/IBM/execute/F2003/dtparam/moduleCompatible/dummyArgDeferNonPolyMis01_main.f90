!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferNonPolyMis01.f
!*
!*  DATE                       : Nov. 12 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  12.4.1.7 :
!*  While an entity is associated with a dummy argument, following restrictions hold:
!*  1) Action that affects the allocation status of the entity or a subobject thereof shall be taken throgh the dummy argument. Action that affects the value of the entity or any subobject of it shall be taken only through the dummy argument unless:
!* a) the dummy argument has the POINRER attribute or
!* b) the dummy argument has TARGET attribute, the dummy argument does not have INTENT(IN), the dummy argument is a scalar object or an assumed shape array, and the actual argument is a target other than an array section with a vector subscript.
! dummy argument in following test case has TARGET attribute.
!234567890123456789012345678901234567890123456789012345678901234567890

program dummyArgDeferNonPolyMis01
  use m
  implicit none

  call outer

end program

subroutine set(arg1,arg2)
   use m,only : dtp
   type(dtp(:,:)),target,allocatable,intent(inout) :: arg1(:)
   type(dtp(*,*)),intent(in) :: arg2

   arg1(lbound(arg1,1))=arg2

end subroutine
