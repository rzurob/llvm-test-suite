!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d361522.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Jan. 26 2009 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  Defect 361522
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(len)
     integer,len  :: len
     integer      :: i1(len:len)=-99 
  end type

  type base(l1)
    integer,len :: l1
    type(A(l1))  :: a3comp(l1) 
  end type
end module

program d361522

  use m
  class(base(:)),allocatable :: pobj1

  allocate(base(1) :: pobj1)
  open(10,file='d361522.dat')

  select type(pobj1)     
     type is(base(*))
        print *,"start to read"
        read(10,*) pobj1%a3comp  
        print *,"end of read"
        print *,pobj1%a3comp
     class default
        stop
  end select

  close(10)

end program
