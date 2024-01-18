!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 24 2009
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!*  defect 361331
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k2,l2)
     integer,kind :: k2
     integer,len  :: l2
     contains
       procedure :: readDT=>readBase
       generic   :: myread=>readDT
   end type

  type,extends(base) :: child(k3,l3)
     integer,kind :: k3
     integer,len  :: l3

     integer(k3) :: i1(l3-1)
     contains
        procedure :: readDT=>readChild
        generic  :: myread=>readDT
  end type

  contains
       subroutine readBase(this,unit)
          class(base(8,*)),intent(inout) :: this
          integer,intent(in) :: unit

          print *,"in readBase"
          print *,this%k2,this%l2
       end subroutine

    subroutine readChild(this,unit)
       class(child(8,*,4,*)),intent(inout) :: this
       integer,intent(in)  :: unit

       print *,"in readChild"
       print *,this%k2,this%l2,this%k3,this%l3
       call this%base%myread(unit)

    end subroutine

end module

program d361331
   use m

   interface
      subroutine sub(arg)
        import
        class(base(8,*)),intent(inout) :: arg(2:)
      end subroutine
   end interface

   type(child(8,4,4,3)),target,allocatable :: tar(:)

   allocate(tar(-1:0))

   print *,tar%k2,tar%l2,tar%k3,tar%l3

   call sub(tar)

end program

subroutine sub(arg)
   use m
   class(base(8,*)),intent(inout) :: arg(2:)


   open(10,status='scratch')

   call arg(2)%myread(10)

   close(10)

end subroutine
