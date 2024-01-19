!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 24 2009
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!*  defect 361392
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
     integer,len  :: l1
     character(4) :: c1(l1:l1+1,l1:l1+1)="A"
   end type
end module

program d361392
  use m
  class(base(3)),pointer :: ptr=>null()

  allocate(base(3) :: ptr)
  select type(ptr)
     type is(base(*))
      if(ptr%c1(3,4) /= "A" ) error stop 1
     class default
        stop 2
  end select

end program
