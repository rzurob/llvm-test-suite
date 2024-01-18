!*********************************************************************
!*  ===================================================================
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
!*  defect 359818
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1)
     integer,len :: l1
     character(l1) :: c1(l1:l1)
  end type
  contains
     subroutine sub(arg)
        type(base(*)),target,intent(in)  :: arg(:)
        print *,arg
     end subroutine
end module

program d359818

  use m
  implicit none

  type(base(3)),target :: tbase1(2:3)
  tbase1=[base(3)(c1=["xlf"]), &
          base(3)(c1=["abc"])]
  call sub(tbase1)

end program

