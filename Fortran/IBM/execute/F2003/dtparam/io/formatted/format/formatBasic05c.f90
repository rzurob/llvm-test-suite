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
!* 1. test READ & WRITE statement with different edit descriptors
!* 2. derived type is polymorphic type
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k1,l1)
      integer,kind :: k1
      integer,len  :: l1
      real(k1)     :: r1(l1) !l1=2
   end type
   type,extends(base) :: child(l2)
      integer,len  :: l2
      complex(k1+k1) :: x1(l2) !l2=3
   end type
   type,extends(child) :: gen3(k3,l3)
      integer,kind  :: k3
      integer,len   :: l3
      type(child(k3,l3,l3+1)) :: comp !k3=4,l3=2
   end type

   contains
      subroutine readdata(arg)
         class(base(4,*)),pointer,intent(inout) :: arg(:)
         integer :: ios

         allocate(gen3(k1=4,l1=2,l2=3,k3=4,l3=2) :: arg(2:2))

         open(10,file='formatBasic05c.in',action='read',status='old',&
              form='formatted',blank='zero',sign='plus', &
              decimal='comma',iostat=ios)

         if(ios /= 0) then
              write(*,*) "fail to open the file"
              write(*,*) "iostat=",ios
              write(*,*) "iomsg=",msg
              error stop 102_4
         else
             select type(arg)
                type is(gen3(k1=4,l1=*,l2=*,k3=4,l3=*))

                    read(10,fmt=100)  arg(2)%r1,arg(2)%x1

                    write(*,*) "first write"
                    write(*,'(f8.3/e9.3)') arg(2)%r1
                    write(*,'(f6.1/e8.3/e15.5/e15.5,/e15.3e3/f4.0)') arg(2)%x1

                    rewind 10

                    associate(x=>arg(2)%comp)

                      read(10,fmt=101) x

                      write(*,*) "second write"
                      write(*,'(f8.3/e9.3)') x%r1
                      write(*,'(f6.1/e8.3/e15.5/e15.5,/e15.3e3/f4.0)') x%x1

                    end associate

                   100 format(f8.3/f7.2,/f6.1/e8.1,/f9.1/f9.1,/f8.3/f4.0)
                   101 format(en8.3/g7.2,/g6.1e2/es8.1,/g9.1/e9.1,/g10.3e3/g4.0)
                class default
                    error stop 100_4
             end select
         end if

      end subroutine
end module

program formatBasic05c
  use m
  implicit none

  class(base(k1=4,l1=2)),pointer :: pbase1(:)=>null()
  class(base(k1=4,l1=:)),pointer :: pbase2(:)=>null()


  call readdata(pbase1)

  pbase2(0:)=>pbase1

  if(lbound(pbase1,1) /= 2)                           error stop 10_4
  if(ubound(pbase1,1) /= 2)                           error stop 11_4

  if(lbound(pbase2,1) /= 0)                           error stop 12_4
  if(ubound(pbase2,1) /= 0)                           error stop 13_4

  select type(pbase2)
     type is(gen3(k1=4,l1=*,l2=*,k3=4,l3=*))

       write(*,*) "third write"
       write(*,fmt=102) pbase2

       102 format(f8.3/e9.3/f6.1/e8.3/e15.5/e15.5,/e15.3e3/f4.0)

     class default
         error stop 101_4
  end select

  close(10)

  deallocate(pbase1)

end program
