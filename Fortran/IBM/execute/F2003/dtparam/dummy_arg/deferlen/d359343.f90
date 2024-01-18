!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d359343.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Nov. 24 2008 
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
!*  DEFECT 359343 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
      integer,len  :: l1
      character(l1) :: c1
      contains
         procedure,pass :: assign=>assignbase
         generic :: assignment(=) => assign
   end type

   type,extends(base) :: child(l2)
      integer,len  :: l2
      type(base(l1)),pointer :: base1=>null()
   end type

   contains
      subroutine assignbase(this,arg)
         class(base(*)),intent(inout) :: this
         class(base(*)),intent(in)    :: arg
      end subroutine

      subroutine assignchild(this,arg)
         class(child(*,*)),intent(inout) :: this
         class(base(*)),intent(in)    :: arg

      end subroutine

end module

program d359343
  use m
  implicit none

end
