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

