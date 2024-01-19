!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 6 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. DEFECT 357082
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
      integer,len :: l1
      character(l1),allocatable :: ch2
   end type
   type,extends(base) :: child
      class(base(3)),pointer :: baseComp=>null()
   end type
end module

program d357082
  use m
  implicit none

  class(base(3)),pointer :: base1
  allocate(child(3) :: base1)

  base1%ch2="xlf"
  select type(base1)
     type is(child(*))
        allocate(base1%baseComp,source=base1)
        if(base1%l1 /= 3)                          error stop 10_4
        if(base1%ch2 /= "xlf")                     error stop 11_4
        select type(x=>base1%baseComp)
           type is(child(*))
              if(x%l1 /= 3)                        error stop 12_4
              if(x%ch2 /= "xlf")                   error stop 13_4
           class default
              error stop 100_4
        end select
  end select

end program

