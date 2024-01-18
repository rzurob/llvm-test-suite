!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d360170.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Dec. 15 2008 
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
!*  defect 360170
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
      integer,len   :: l1
      integer       :: int(l1)
   end type

   type,extends(base) :: child(l2)
      integer,len     :: l2
      character(7)    :: char(1:2)
   end type
end module

program d360170
  use m
  implicit none

  class(base(3)),pointer :: base1(:)=>null()

  allocate(child(3,4) :: base1(1:1))

  select type(base1)
      type is(child(*,*))
            base1(1)%int=[11,12,13]
            print *,base1(1)%int
            base1(1)%char=["Markham","Toronto"]
            print *,base1(1)%int
      class default
         stop 11
  end select

end program 
