!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 12 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : INTRINSICS(MERGE)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.75
!* 2. INTRINSICS:MERGE(TSOURCE,FSOURCE,MASK)
!* 3. DEFECT 356111
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(l1)
     integer,len  :: l1=3
     character(:),pointer :: c1(:)=>null()
  end type
  type,extends(A) :: B(l2)
     integer(2),len :: l2=4
  end type
end module

program d356111
   use m
   implicit none

   class(A(:)),pointer      :: a2(:)
   class(A(:)),pointer      :: b2(:)

   character(7),target      :: ch1(4)=["ab","cd","ef","gh"]

   allocate(a2(2),source=[B(4,6)(ch1(1:2)),B(4,6)(ch1(3:4))])
   allocate(b2(2),source=[B(4,6)(ch1(3:3)),B(4,6)(ch1(4:4))])

   call t (merge(a2,b2,.true.))
   contains
   subroutine t (x)
        class(A(*)) x(:)
        print *, associated(x(1)%c1, x(2)%c1)
        print *,"|",x(1)%c1,"|",x(2)%c1,"|"
   end subroutine
end


