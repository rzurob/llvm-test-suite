!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 14 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  DEFECT 357409
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type mychar(l1)
    integer,len    :: l1
    character(l1) :: ch1
  end type

  type container(l3)
    integer,len    :: l3
    character(l3) :: ch2
    type(mychar(l3)) :: mychar1
  end type
end module

program d357409
  use m
  implicit none
  type(container(:)),pointer :: contain3(:)=>null()

  allocate(contain3(3),source=container(3)("123",mychar(3)(ch1="xlf")) )
  if(any(contain3%mychar1%ch1 /= "xlf"))    stop

end program
