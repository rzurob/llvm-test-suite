!*********************************************************************
!*  ===================================================================
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
!* 1. defect 361893
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type A(l1)
      integer,len :: l1
      character(l1),allocatable :: c1(:)
   end type
   type B(l2)
      integer,len :: l2
      type(A(l2+1)),allocatable :: a1comp(:)
   end type
end module

program d361493
     use m
     type(B(2))  :: cobj(2:3)

     call allocComp(cobj)
     contains
          subroutine allocComp(dt)
               type(B(*)),intent(inout) :: dt(:)
               integer :: l
               l=lbound(dt,1)
               print *,l
               allocate(dt(l)%a1comp(2))

          end subroutine

end program
