!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 9 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1.test READ statement in type bound procedure
!* 2.derived type has mulitiple intrinsic ultimate components
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type inner(l1)
     integer(8),len  :: l1
     integer         :: i1(l1) !l1=3
     character(l1+1) :: c1(l1)
   end type

   type outer(l2)
     integer,len  :: l2
     logical      :: log1(l2) !l2=2
     real         :: r1(l2:l2)!l2=2
     complex(8)   :: x1
     type(inner(l2+1)) :: comp(l2)
     contains
        procedure,pass :: readOuter
        procedure,pass :: writeOuter
   end type

   contains

       subroutine readOuter(this)
           class(outer(*)),intent(inout) :: this

           select type(this)
             type is(outer(*))
               read(10,100) this
               100 format(2l5/es10.4/f10.2,e10.2e2/2i5.3,i5/3a3/bn,3i5/t3,3a3)
             class default
               stop 10
           end select
       end subroutine

       subroutine writeOuter(this)
           class(Outer(*)),intent(in) :: this

           select type(this)
             type is(outer(*))
               write(*,101) this
               101 format(2l5/e15.4/2e15.4/2(3i5/"|",a3,"|",a3,"|",a3,"|",:/))
             class default
               stop 11
           end select

       end subroutine

end module

program formatTypeBndSeqRead02
  use m
  implicit none

  class(outer(:)),pointer :: outer1=>null()

  integer :: ios
  character(500) :: msg

  allocate(outer(2) :: outer1)

  open(10,file="formatTypeBndSeqRead02.dat",action='read',&
       form='formatted',access='sequential',blank='zero',decimal='comma',&
       sign='plus',iomsg=msg,iostat=ios)

  if(ios /= 0) then
     print *,"fail to open the file"
     print *,"iomsg=",msg
     print *,"iostat=",ios
     stop 9
  else
     call outer1%readOuter
     call outer1%writeOuter
  end if

  close(10)

end program
