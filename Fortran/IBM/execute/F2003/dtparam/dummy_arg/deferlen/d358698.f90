!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d358698.f
!*
!*  DATE                       : Nov. 10 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  DEFECT 358698
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(l1)
     integer,len   :: l1
     character(3)  :: c1
   end type
end module

program d358698
  use m
  implicit none

  type(A(3)),allocatable       :: a1(:)
  character(3),allocatable     :: c1(:)

  allocate(c1(2:3),source=["xlf","xlc"])

  allocate(a1(2:3),source= &
         [A(3)("xlf"),A(3)("xlc")])

  call sub(a1,c1)
  contains
    subroutine sub(arg1,arg2)
       type(A(*)),intent(in)   :: arg1(1:)
       character(*),intent(in) :: arg2(1:)

       print *,shape(arg1),lbound(arg1,1),ubound(arg1,1)
       print *,arg1(1),arg1(2)
       print *,shape(arg2),lbound(arg2,1),ubound(arg2,1)
       print *,arg2(1),arg2(2)
    end subroutine

end program

