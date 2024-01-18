!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d361413.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Jan. 24 2009 
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
!*  DESCRIPTION
!*
!*  defect 361413
!234567890123456789012345678901234567890123456789012345678901234567890

program d361413

  type base(l1)
     integer,len  :: l1
     character(3) :: c1(l1:l1,l1:l1)="***"
  end type

  class(base(3)),pointer :: ptr=>null()

  allocate(base(3) :: ptr)
  select type(ptr)         
     type is(base(*))

       print *,ptr%c1(3,3)
     class default
        stop 2
  end select

end program

