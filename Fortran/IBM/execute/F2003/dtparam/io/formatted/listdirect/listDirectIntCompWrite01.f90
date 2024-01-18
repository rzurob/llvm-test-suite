!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : listDirectIntCompWrite01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Jan. 8 2009 
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
!* 1. Derived type is polymorphic and has ultimate integer components
!* 2. Test write statement inside typebound procedure
!234567890123456789012345678901234567890123456789012345678901234567890
module m1
   type,public :: base(k1,l1) !(4,3)
     integer,kind :: k1 
     integer,len  :: l1

     integer(k1)  :: i1(l1)
     integer(k1)  :: i2

     contains
       procedure  :: writeDT=>writeBase
       generic    :: write=>writeDT
   end type

   contains

       subroutine writeBase(this,unit)
          class(base(4,*)),intent(in) :: this
          integer,intent(in)  :: unit
          integer :: ios
          character(256) :: msg

          print *,"in writeBase"
          select type(this)
             type is(base(4,*))
               write(unit,fmt=*,delim='apostrophe',decimal='comma', &
                          iostat=ios,iomsg=msg)  this
             class default
               stop 12
          end select
       end subroutine
end module

module m2
  use m1
  type,public,extends(base) :: child(k2,l2) !(4,5)
     integer,kind :: k2
     integer,len  :: l2     
     integer(k1+k2)  :: i3(l1:l2)

     contains
       procedure :: writeDT=>writeChild
       generic   :: write=> writeDT
       procedure :: modChild=>modifyChild
  end type

  contains

   subroutine writeChild(this,unit)
      class(child(4,*,4,*)),intent(in) :: this
      integer,intent(in)  :: unit
      integer :: ios
      character(256) :: msg
  
      print *,"in writeChild" 
      select type(this) 
         type is(child(4,*,4,*))
            call this%base%write(unit)
               write(unit,fmt=*,iostat=ios,iomsg=msg)  this 
         class default
           stop 11
      end select
   end subroutine

   function modifyChild(this)
      class(child(4,*,4,*)),intent(in) :: this
      class(child(4,this%l1,4,this%l2)),allocatable :: modifyChild
      
      allocate(modifyChild,source=this)
      modifyChild%i1=-this%i1
      modifyChild%i2=-this%i2
      modifyChild%i3=-this%i3

   end function

end module


program listDirectIntCompWrite01
  use m2
  implicit none

  integer :: ios,i
  integer,parameter :: unit=1025
  character(256) :: msg

  class(base(4,:)),pointer :: base1(:)=>null()
  class(child(4,:,4,:)),allocatable,target :: child1(:)

  allocate(child1(2), &
          source=[child(4,3,4,5)(i1=[11,12,13],i2=14,i3=[15,16,17]), &
                  child(4,3,4,5)(i1=[21,22,23], i2=24,i3=[25,26,27])] )

  ! base1 is reverse of child1
  base1(3:)=>child1(2:1:-1)

  open(unit,file='listDirectIntCompWrite01.out',sign='plus',&
       form='formatted',access='sequential',iostat=ios,iomsg=msg)

  if(ios <> 0) then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop 10
  end if 

  do i=lbound(child1,1),ubound(child1,1)
     call child1(i)%write(unit)
      associate(x=>child1(i)%modChild())
       call x%write(unit)
     end associate
  end do  

  do i=lbound(base1,1),ubound(base1,1)
     call base1(i)%write(unit)
     select type(x=>base1(i))
       type is(child(4,*,4,*))
        associate(y=>x%modChild())
           call y%write(unit)
        end associate
       class default
         stop 13
     end select
  end do 
   
  close(unit,status='keep')
end program
