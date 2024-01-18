!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatInternalFile05.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Dec. 15 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : FORMATTED INTRINSIC IO 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  1. test READ or WRITE from or to internal file
!*  2. use direct access
!*  3. derived type has type bound procedure, and read & write inside type bound procedure
!*  4. internal file is default character array pointer
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k1,l1)
      integer,kind  :: k1  ! k1=2
      integer,len   :: l1  ! l1=3
      character(l1) :: c1(l1-1)
      integer(k1)   :: i1(l1)
      logical(k1)   :: g1(l1-1:l1+1)
      real(2*k1)    :: r1(l1-1)
      contains
        procedure,pass :: writeInternal1
        procedure,pass :: readInternal1 
        procedure,pass :: writeInternal2
        procedure,pass :: readInternal2
        procedure,pass :: writeData1
        procedure,pass :: writeData2
   end type

   contains
  
      subroutine writeInternal1(this,buffer,unit)
         class(dtp(2,*)),intent(in) :: this
         character(32),intent(inout) :: buffer(1:*)
         integer,intent(in) :: unit  

         read(unit,'(a)',rec=5) buffer(lbound(buffer,1)) 
         read(unit,'(a)',rec=6) buffer(lbound(buffer,1)+1)
         read(unit,'(a)',rec=7) buffer(lbound(buffer,1)+2)
         read(unit,'(a)',rec=3) buffer(lbound(buffer,1)+3)
        
      end subroutine

      subroutine writeInternal2(this,buffer,unit)
         class(dtp(2,*)),intent(in) :: this
         character(32),intent(inout) :: buffer(5:*)
         integer,intent(in) :: unit

         read(unit,'(a)',rec=15) buffer(lbound(buffer,1))
         read(unit,'(a)',rec=16) buffer(lbound(buffer,1)+1)
         read(unit,'(a)',rec=17) buffer(lbound(buffer,1)+2)
         read(unit,'(a)',rec=13) buffer(lbound(buffer,1)+3)

      end subroutine

      subroutine readInternal1(this,buffer)
         class(dtp(2,*)),intent(inout) :: this
         character(32),intent(in) :: buffer(1:*)
         integer :: i
         select type(this)
           type is(dtp(2,*)) 
                read(buffer(1), '(2a3)') this%c1
                read(buffer(2), '(3i4)') this%i1
                read(buffer(3), '(3l3)') this%g1
                read(buffer(4), '(3f9.3)') this%r1
           class default
             stop 11
         end select
    
      end subroutine

      subroutine readInternal2(this,buffer)
         class(dtp(2,*)),intent(inout) :: this
         character(32),intent(in) :: buffer(5:*)

         select type(this)
           type is(dtp(2,*))
              read(buffer(5), '(2a3)') this%c1
              read(buffer(6), '(3i4)') this%i1
              read(buffer(7), '(3l3)') this%g1
              read(buffer(8), '(2f9.3)') this%r1
           class default
             stop 12
         end select

      end subroutine

      subroutine writeData1(this,unit)
         class(dtp(2,*)),intent(in) :: this
         integer,intent(in) :: unit
     
         write(unit,'(2a3)',rec=5) this%c1
         write(unit,'(3i4)',rec=3) this%i1
         write(unit,'(3l3)',rec=7) this%g1
         write(unit,'(2f9.3)',rec=3) this%r1
         write(unit,'(3i4)',rec=6) this%i1
 
      end subroutine

     subroutine writeData2(this,unit)
        class(dtp(2,*)),intent(in) :: this
        integer,intent(in) :: unit

        write(unit,'(2a3)',rec=15) this%c1
        write(unit,'(3i4)',rec=13) this%i1
        write(unit,'(3l3)',rec=17) this%g1
        write(unit,'(2f9.3)',rec=13) this%r1
        write(unit,'(3i4)',rec=16) this%i1

     end subroutine

end module

program formatInternalFile05
  use m
  implicit none

  integer :: ios
  character(300) :: msg

  type(dtp(2,:)),allocatable :: dtp1(:)
  type(dtp(2,3)) :: dtp2(2)

  character(:),pointer :: buffer(:)=>null()

  allocate(character(32) :: buffer(8))

  allocate(dtp(2,3) :: dtp1(-1:0))

  dtp1(-1)%c1=["IBM","XLF"]
  dtp1(-1)%i1=[11,12,13]
  dtp1(-1)%g1=[.true.,.false.,.true.]
  dtp1(-1)%r1=[4.153,-12.67]

  dtp1(0)%c1=["POT","CUP"]
  dtp1(0)%i1=[-11,-12,-13]
  dtp1(0)%g1=[.false.,.true.,.false.]
  dtp1(0)%r1=[0.12E-1,3.4E-02]

!  write(*,'(a/)') "Value of dtp1:"
  write(*,'(2a3/3i4/3l3/2f9.3)') dtp1

  open(10,file='formatInternalFile05.record',form='formatted',status='replace',&
          action='readwrite',access='direct',recl=32,iostat=ios,iomsg=msg)

  if(ios /= 0) then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop 10 
  else

     call dtp1(-1)%writeData1(10)
     call dtp1(0)%writeData2(10)

     call dtp1(-1)%writeInternal1(buffer(:4),10)
     call dtp1(0)%writeInternal2(buffer(5:),10)

!     write(*,'(/a/)') "Value of buffer:"
     write(*,'(a)') buffer
 
     call dtp2(1)%readInternal1(buffer(:4)) 
     call dtp2(2)%readInternal2(buffer(5:)) 

!     write(*,'(/a/)') "Value of dtp2:"
     write(*,'(2a3/3i4/3l3/2f9.3)') dtp2
  
  end if

  close(10)  

end program
