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
!* 3. DEFECT 356159
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(l1)
       integer, len  :: l1
       character(l1) :: ca(l1)
   end type
   type B(l2)
       integer,len   :: l2
       character(l2) :: cb(l2)
       type(A(:)),allocatable :: type1
       type(A(:)),pointer     :: type2=>null()
   end type
end module

program d356159
   use m
   implicit none

   type(A(4)) :: a1=A(4)(ca="123")
   type(B(3)) :: b1=B(3)(cb="xlf",type1=null())

   b1%type1=a1
   allocate(b1%type2,source=A(4)(ca="456"))

   call associate_replacer(merge(b1%type1,b1%type2,.true.))
!   associate(x=>merge(b1%type1,b1%type2,.true.))
!      if(x%l1 /= 4)                                  error stop 10_4
!      if(any(x%ca /= "123"))                         error stop 11_4
!      if(size(x%ca,1) /= 4)                          error stop 12_4
!      if(x%ca%len /= len(x%ca) .or. x%ca%len /= 4)   error stop 13_4
!   end associate

    contains

    subroutine associate_replacer(x)
        type(A(*)), intent(in) :: x

      if(x%l1 /= 4)                                  error stop 10_4
      if(any(x%ca /= "123"))                         error stop 11_4
      if(size(x%ca,1) /= 4)                          error stop 12_4
      if(x%ca%len /= len(x%ca) .or. x%ca%len /= 4)   error stop 13_4
    end subroutine
end program

