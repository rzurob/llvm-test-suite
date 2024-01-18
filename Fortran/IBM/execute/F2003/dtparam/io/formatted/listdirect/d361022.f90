!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d361022.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Jan. 16 2009 
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
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(l)
     integer,len   :: l
     integer   :: i(l)
  end type
  contains
      subroutine read(unit,obj)
         integer,intent(in) :: unit
         type(dtp(*)),intent(inout) :: obj(:) 

         read(unit,fmt=*) obj(lbound(obj,1))%i,obj(ubound(obj,1))%i
         print *,obj(lbound(obj,1))%i,obj(ubound(obj,1))%i
         print *,obj
      end subroutine
end module

program d361022

  use m
  implicit none

  integer :: i

  type(dtp(5)),allocatable :: obj(:)
  allocate(dtp(5) :: obj(-1:0) )

  do i=-1,0
    obj(i)%i=-99
  end do

  print *,obj(-1)%i,obj(0)%i

  open(10,file='d361022.dat')

  call read(10,obj)

  close(10)

end program
