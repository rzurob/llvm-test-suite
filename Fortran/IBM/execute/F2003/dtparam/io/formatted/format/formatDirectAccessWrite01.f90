!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatDirectAccessWrite01.f
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
!* 1. test Write statement with direct access
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

      subroutine writeData(dt,unit)
         class(base(2,*)),intent(in) :: dt(:)
         integer,intent(in) :: unit

         select type(dt)
            type is(gen3(2,*,*,*))
               write(unit,'("{",sp,3i4,2a7,3l2,"}")',rec=8) dt(ubound(dt,1))
               write(unit,'("{",3i4,2a7,3l2,"}")',rec=7) dt(lbound(dt,1))
               write(unit,'("{",sp,3i4,2a7,"}")',rec=4)  &
                   dt(ubound(dt,1))%child,dt(lbound(dt,1))%child
               write(unit,'("{",3i4,"}")',rec=2)  &
                   dt(lbound(dt,1))%base,dt(ubound(dt,1))%base
               write(unit,'("{",a,"}")',rec=1) "This is first record"
               write(unit,'("{",a,"}")',rec=6) "This is sixth record"
               write(unit,'("{",a,"}")',rec=6) "THIS IS MODIFIED SIXTH RECORD"
               write(unit,'("{",3i6.4/2a7/3l3,"}")',rec=9) dt
               write(unit,'("{",a,3l1,"}")',rec=11) &
                  "overwrite 11th record: ",dt(lbound(dt,1))%log
               write(unit,'("{",a,"}")',rec=15) "This is 15th record"
        class default
           stop 12
     end select

      end subroutine
end module

program formatDirectAccessWrite01
  use m
  implicit none

  integer        :: ios
  character(300) :: msg

  class(base(2,:)),pointer :: base1(:)=>null()

  allocate(gen3(2,3,4,5) :: base1(-1:0))

  select type(base1)
      type is(gen3(2,*,*,*))
         base1(-1)%int=[11,12,13]
         base1(-1)%char=["fortran","xlftest"]
         base1(-1)%log=[.true.,.false.,.true.]
         base1(0)%int=[-11,-12,-13]
         base1(0)%char=["MARKHAM","TORONTO"]
         base1(0)%log=[.false.,.true.,.false.]
      class default
         stop 11
  end select

  open(10,file="formatDirectAccessWrite01.out",form='formatted',&
       status='new',action='write',access='direct', &
       recl=100,iostat=ios,iomsg=msg)


  if(ios /= 0) then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop  10
  else
     call writeData(base1,10)
  end if

  close(10)

end program
