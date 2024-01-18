!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 15 2008
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
!* 3. DEFECT 356156
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(l1)
       integer, len  :: l1
   end type
   type B(l2)
       integer,len   :: l2
       type(A(:)),allocatable :: type1
   end type
end module

program d356156
   use m
   implicit none
   type(B(3)) :: b1=B(3)(null())

   b1%type1=A(4)()
   call associate_replacer(merge(b1%type1,A(4)(),.true.))
!   associate(x=>merge(b1%type1,A(4)(),.true.))
!       if(x%l1 /= 4)                                  error stop 10_4
!   end associate

    contains

    subroutine associate_replacer (x)
        type(A(*)), intent(in) :: x

        if(x%l1 /= 4)                                  error stop 10_4
    end subroutine
end program

