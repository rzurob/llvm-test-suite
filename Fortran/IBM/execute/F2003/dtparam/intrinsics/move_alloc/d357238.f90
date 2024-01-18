!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 8 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. DEFECT 357238
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type B(l1)
     integer,len   :: l1
     integer       :: i1(l1)
     procedure(),nopass,pointer :: procptr=>null()
  end type
  contains
     subroutine sub()
         print *,"in sub"
     end subroutine
end module

program d357238
  use m
  implicit none

  type(B(:)),allocatable   :: from1,to1

  allocate(from1,source= B(2)([4,5]))

  from1%procptr=>sub

  print *,allocated(from1),associated(from1%procptr)
  call from1%procptr

  call move_alloc(from1,to1)

  print *,associated(to1%procptr,sub)
  call to1%procptr()

end program

