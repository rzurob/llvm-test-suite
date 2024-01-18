!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 16 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. DEFECT 356228
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type base(l1)
       integer,len   :: l1
       character(l1) :: c1(2)
    end type
    type,extends(base) :: child
       class(base(:)),pointer     :: base1=>null()
    end type

end module

program d356228
   use m
   implicit none

   class(base(:)),allocatable  :: b1
   class(base(:)),pointer      :: b2
   type(child(4)),target     :: ch1,ch2

   ch1=child(4)(c1=["ab","cd"])
   ch2=child(4)(c1=["12","34"])

   ch1%base1=>ch2
   ch2%base1=>ch1

   b2=>ch2
   allocate(b1,source=ch1)

   select type(x=>b1)
      type is(child(*))
         select type(y=>x%base1)
            type is(child(*))
              if(y%l1 /= 4)                                  error stop 10_4
              if(any(y%c1 /= ["12","34"]))                   error stop 11_4
              if(y%c1%len /= len(y%c1) .or. y%c1%len /= 4)   error stop 12_4
            class default
              error stop 100_4
         end select
   end select

end program


