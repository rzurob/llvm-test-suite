!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d361743_1.f
!*
!*  DATE                       : Feb. 5 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. defect 361743
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type base(l1)
      integer,len :: l1
      character(3)   :: c1(3)="***"
   end type

   type,extends(base) :: child
      integer  :: i1(3)=-99
   end type
   contains
      subroutine sub(dt)
         class(base(3)),intent(in)  :: dt

         if(any(dt%c1 /= "xlf"))   stop 10
      end subroutine
end module

program d361743_1
     use m
     type(child(3)),allocatable :: obj(:)

     allocate(obj(3))

     obj=child(3)(c1="xlf",i1=-10)

     call sub(obj(1)%base)

     if(any(obj(1)%i1 /= -10))      stop 11

end program
