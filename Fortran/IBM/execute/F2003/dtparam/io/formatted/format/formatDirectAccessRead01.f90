!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatDirectAccessRead01.f
!*
!*  DATE                       : Dec. 14 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : FORMATTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. test Read statement with direct access
!* 2. derived type is polymorphic array
!* 3. test dummy argument
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k1,l1)
      integer, kind :: k1 !k1=2
      integer,len   :: l1 !l1=3
      integer(k1) :: int(l1)
   end type

   type,extends(base) :: child(l2)
      integer,len     :: l2 !l2= 4
      character(l1+l2) :: char(l1:l2)
   end type

   type,extends(child) :: gen3(l3)
      integer,len     :: l3 !l3=5
      logical(k1)     :: log(l3:l1+l2)
   end type

   contains

      subroutine readData(dt,unit)
         class(base(2,:)),pointer,intent(out) :: dt(:)
         integer,intent(in) :: unit

         allocate(gen3(2,3,4,5) :: dt(-1:0))

         select type(dt)
            type is(gen3(2,*,*,*))

                 read(10,'(3l4)',rec=5)  dt(-1)%log
                 read(10,'(sp,bz,3i4)',rec=3) dt(-1)%int
                 read(10,'(2a7)',rec=4) dt(-1)%char

                 read(10,'(3l6)',rec=10) dt(0)%log
                 read(10,'(3i4)',rec=9) dt(0)%int
                 read(10,'(2a7)',rec=8) dt(0)%char

        class default
           stop 11
     end select

      end subroutine
end module

program formatDirectAccessRead01
  use m
  implicit none

  integer        :: ios
  character(300) :: msg

  class(base(2,:)),pointer :: base1(:)=>null()

  open(10,file="formatDirectAccessRead01.in",form='formatted',&
       status='old',action='read',access='direct', &
       recl=20,iostat=ios,iomsg=msg)

  if(ios /= 0) then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop 10
  else
     call readData(base1,10)
     select type(base1)
        type is(gen3(2,*,*,*))
           write(*,'(3i4/2a7/3l4)') base1
        class default
           stop 12
     end select
  end if

  close(10)

end program
