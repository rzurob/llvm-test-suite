!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatTypeBndSeqReadWrite01.f
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
!* 1. test WRITE & READ statement
!* 2. write & read in same file
!* 3. derived type is polymorphic, use type bound procedure
!234567890123456789012345678901234567890123456789012345678901234567890
module m

  integer,parameter :: COMP=4,DT=5

  type base(l1)
     integer,len :: l1
     character(l1) :: c1(l1-1:l1+1) ! l1=2
     logical       :: log1(l1)
     contains
        procedure,pass :: writeBase
        procedure,pass :: readBase
  end type

  type,extends(base) :: child(l2,l3)
     integer,len :: l2,l3
     character(l1+l2+l3) :: c2(l2:l3) !l2=3, l3=4
     logical       :: log2(l1:l2)
     contains
        procedure,pass :: writeChild
        procedure,pass :: readChild
  end type

  contains

     subroutine writeChild(this,flag)
        class(child(*,*,*)),intent(in) :: this
        integer,intent(in) :: flag

        select type(this)
           type is(child(*,*,*))
              if(flag .eq. COMP) then
                 call this%base%writeBase
                 write(10,'(2a9,/2l4)') this%c2,this%log2
              else
                 write(10,'(3a2,/2l4/2a9/2l4)') this
              end if
           class default
              stop 12
        end select

     end subroutine

     subroutine writeBase(this)
        class(base(*)),intent(in) :: this

        select type(this)
           type is(base(*))
              write(10,'(3a2/2l4)') this
           class default
              stop 13
        end select
     end subroutine
     subroutine readChild(this,flag)
        class(child(*,*,*)),intent(inout) :: this
        integer,intent(in) :: flag

        select type(this)
           type is(child(*,*,*))
              if(flag .eq. COMP) then
                 call this%base%readBase
                 read(10,'(2a9,/2l4)') this%c2,this%log2
              else
                 read(10,'(3a2,/2l4/2a9/2l4)') this
              end if
           class default
              stop 16
        end select

     end subroutine
     subroutine readBase(this)
        class(base(*)),intent(inout) :: this

        select type(this)
           type is(base(*))
              read(10,'(3a2/2l4)') this
           class default
              stop 17
        end select

     end subroutine

end module

program formatTypeBndSeqReadWrite01
  use m
  implicit none

  integer :: ios
  character(500) :: msg

  class(base(:)),allocatable :: base1(:)

  allocate(child(2,3,4) :: base1(-3:-2))

  select type(x=>base1)
     type is(child(*,*,*))
            x(-3)%c1=["ab","cd","ef"]
            x(-2)%c1=["12","34","56"]
            x(-3)%log1=[.false.,.true.]
            x(-2)%log1=[.true.,.false.]
            x(-3)%c2=["IBMCANADA","XLFORTRAN"]
            x(-2)%c2=["test team","front end"]
            x(-3)%log2=.true.
            x(-2)%log2=.false.
     class default
         stop 10
  end select

  open(10,file="formatTypeBndSeqReadWrite01.out",action="readwrite",&
       form="formatted",access="sequential",iomsg=msg,iostat=ios)

  if(ios /= 0) then
     print *,"fail to open the file"
     print *,"iomsg=",msg
     print *,"iostat=",ios
     stop 9
  else

     select type(base1)
        type is(child(*,*,*))

          call base1(-3)%writeChild(COMP)
          call base1(-2)%writeChild(DT)
        class default
           stop 11
     end select
  end if

  rewind 10

  deallocate(base1)

  if(allocated(base1))      stop 14

  allocate(child(2,3,4) :: base1(2))

  select type(base1)
    type is(child(*,*,*))
         call base1(1)%readChild(COMP)
         call base1(2)%readChild(DT)

      class default
        stop 15
  end select

  select type(base1)
    type is(child(*,*,*))
         call base1(1)%writeChild(COMP)
         call base1(2)%writeChild(DT)

      class default
        stop 18
  end select

  close(10)

end program
