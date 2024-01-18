!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d359818.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Dec. 5 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :  
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
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

