!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 24 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!234567890123456789012345678901234567890123456789012345678901234567890

module m
  type DT1(l1)
     integer,len  :: l1 ! l1=4
     character(l1) :: c1(l1-1)="****"
     character(l1+1) :: c2="*****"
  end type
  type DT2(l2)
     integer,len  :: l2 ! l2=4
     logical  :: g1=.false.
     logical :: g2(l2)=.false.
     type(DT1(l2)) :: dt1comp
  end type

  contains

    subroutine read(dt2,unit)
       type(DT2(*)),intent(inout) :: dt2
       integer :: mypos,unit

       inquire(unit,pos=mypos)
       read(unit,*,pos=mypos) dt2%g1,dt2%g2(1)
       inquire(unit,pos=mypos)
       read(unit,*,pos=mypos) dt2%g2(2:4)
       inquire(unit,pos=mypos)
       read(unit,*,pos=mypos) dt2%dt1comp%c1
       inquire(unit,pos=mypos)
       read(unit,*,pos=mypos) dt2%dt1comp%c2
       inquire(unit,pos=mypos)
    end subroutine

end module

program d361277

   use m

   type(DT2(4)), allocatable :: dt2
   open(10,file='d361277.dat',form='formatted',access='stream')

   allocate(dt2(4) :: dt2 )

   call read(dt2,10)

   write(*,*) dt2

   close(10)

end program

