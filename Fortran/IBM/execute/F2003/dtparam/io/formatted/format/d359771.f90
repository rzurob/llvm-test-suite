!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d359771.f
!*
!*  DATE                       : Dec. 5 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  defect 359771
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type :: base(l1)
     integer,len    :: l1
     character(l1) :: c1
     logical      :: log1
  end type

  contains
    subroutine readbase(arg1)
       type(base(*)),pointer,intent(inout) :: arg1

       read (10,fmt= '(a8)') arg1%c1
       read (10,fmt= '(l2)') arg1%log1

       write(*,*) arg1%c1,arg1%log1

    end subroutine
end module

program d359771
  use m
  implicit none

  type(base(8)),pointer :: base1

  allocate(base1)

  open(10,file="d359771.in")

  call readbase(base1)

  close(10)

end program

