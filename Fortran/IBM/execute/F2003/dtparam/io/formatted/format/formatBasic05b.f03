!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 8 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. test WRITE statement with edit descriptor:fw.d,ew.d,ew.dee,esw.d,enw.d ..
!* 2. derived type has real & complex ultimate components
!* 3. test dummy argument
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k1,l1)
      integer,kind :: k1
      integer,len  :: l1
      real(k1)     :: r1
      real(k1+k1)  :: r2(k1:k1+1)
      complex(k1+k1) :: x1(l1)
   end type

   contains

      subroutine printbase(arg)
        class(base(4,:)),pointer,intent(in) :: arg

        select type(arg)
           type is(base(4,*))
           write (10,10) arg
           write (unit=10,fmt=11)  arg

10 format(1x,"r1=",:,f12.3,/1x,"r2(4)=",:,e12.3,/1x,"r2(5)=",:,e12.3,/1x,"x1(1)=",:,"(",(e12.3,",",e12.3e5),")",/,1x,"x1(2)=",:,"(",(en12.3,",",en12.3e5),")",/,1x,"x1(3)=","(",(es12.3,",",es12.3e5),")")

11 format(1x,"r1=",:,g12.3,/1x,"r2(4)=",:,g12.3,/1x,"r2(5)=",:,g12.3,/1x,"x1(1)=",:,"(",(g12.3,",",g12.3e5),")",/,1x,"x1(2)=",:,"(",(g12.3,",",g12.3e5),")",/,1x,"x1(3)=","(",(g12.3,",",g12.3e5),")")


         class default
             error stop 100_4
        end select
      end subroutine
end module

program formatBasic05b
  use m
  implicit none

  class(base(4,:)),pointer :: pbase1=>null()

  integer :: ios
  character(256) :: msg

  allocate(pbase1,source= &
   base(4,3)(r1=12.3456,r2=[123456d-100,12.3456d-120], &
         x1=[(123456d+5,123.456d+4),&
             (123456d+5,123.456d+4),&
             (123456d+5,123.456d+4)] ) )

  open(10,file='formatBasic05b.out',action='write',blank='zero',sign='plus', &
        decimal='point',iostat=ios)

  if( ios /= 0) then
     write(10,'(a)')  "fail to open the file"
     write(10,'(a,i5)') "iostat=",ios
     write(10,'(a,a)') "iomsg=",msg
     error stop 101_4
  else
     call printbase(pbase1)
  end if

  close(10)

end program