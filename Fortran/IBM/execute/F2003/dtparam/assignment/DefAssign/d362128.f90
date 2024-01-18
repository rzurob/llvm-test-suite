!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d362128.f
!*
!*  DATE                       : Feb. 11 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. defect 362128
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type A(k1)
     integer,kind :: k1

     integer(k1),pointer :: i1(:,:)=>null()
     contains
         procedure :: assignA1
         procedure :: assignA2
         generic :: assignment(=)=>assignA1,assignA2
   end type

   type base(l1)
     integer,len :: l1
     character(l1),pointer :: c1(:,:)=>null()
     type(A(4)) :: a1comp(2)
   end type

   type,extends(base) :: child(k2)
     integer,kind :: k2
     logical(k2),pointer :: g1(:,:)=>null()
     type(A(2*k2)) :: a2comp
     contains
        procedure :: assignChild1
        procedure :: assignChild2
   end type

   contains

      subroutine assignA1(this,dt)
       class(A(4)),intent(inout) :: this
       type(A(4)),intent(in)     :: dt

     end subroutine

      subroutine assignA2(this,dt)
       class(A(8)),intent(inout) :: this
       type(A(8)),intent(in)     :: dt

     end subroutine

      subroutine assignChild1(this,dt)
       class(child(*,2)),intent(inout) :: this
       type(child(*,2)),intent(in)     :: dt

     end subroutine

      subroutine assignChild2(this,dt)
       class(child(*,4)),intent(inout) :: this
       type(child(*,4)),intent(in)     :: dt

     end subroutine

end module

program d362128
end program
