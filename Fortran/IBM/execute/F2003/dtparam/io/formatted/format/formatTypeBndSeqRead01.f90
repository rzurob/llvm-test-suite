!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatTypeBndSeqRead01.f
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
!* 1. test READ statement in type bound procedure
!* 2. derived type has character & integer ultimate components
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type inner(k1,l1)
     integer(8),kind :: k1
     integer(8),len  :: l1
     integer(k1)     :: i(l1-1:l1+1) ! l1=2
     contains
        procedure,pass :: readInner
   end type

   type outer(k2,l2)
     integer,kind :: k2
     integer,len  :: l2
     character(l2+1)  :: c(k2-1:k2+1) ! k2=4 l2=2
     type(inner(2*k2,l2+1)) :: comp
     contains
        procedure,pass :: readOuter
   end type

   contains

      subroutine readInner(this,unit)
          class(inner(8,*)),intent(inout) :: this
          integer,intent(in) :: unit

          print *,"in readInner"
          select type(this)
             type is(inner(8,*))
               read(unit,'(b16/,i6.3/,g6)') this%i
               write(*,'("|",b16,"|",i6.5,"|",g6.6,"|")') this%i
             class default
               stop 12
          end select

      end subroutine

      subroutine readOuter(this,unit)
          class(outer(4,*)),intent(inout) :: this
          integer,intent(in) :: unit

          print *,"in readOuter"
          select type(this)
             type is(outer(4,*))

               read(unit,'(a5/,a2/,g4.1e2)') this%c
               write(*,'("|",a5,"|",a2,"|",g4.1e2,"|")') this%c

               call this%comp%readinner(unit)
             class default
               stop 11
          end select
      end subroutine

end module

program formatTypeBndSeqRead01
  use m
  implicit none

  type(outer(4,2)),target :: tar1
  type(outer(4,:)),pointer :: outer1=>null()

  integer :: ios
  character(500) :: msg

  outer1=>tar1

  open(10,file="formatTypeBndSeqRead01.dat",iomsg=msg,&
       iostat=ios,blank='zero',sign='suppress')

  if( ios /= 0 )  then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop 10
  else
     call outer1%readOuter(10)
  end if

  print *,"back from readOuter"

  write(*,'("|",a5,"|",a2,"|",g4.1e2,"|",/,"|",b16,"|",i6.5,"|",g6.6,"|")') tar1

  close(10)

end program
