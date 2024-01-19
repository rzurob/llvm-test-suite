!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 21 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. both parent type & child type has nested derived type component
!*  2.dummy arguments are optional allocatable or pointer variable with intent(out) attribute
!*  3. call procedure with actual argument being present or not present.
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dt(l)
      integer(2),len :: l
      character(l)   :: c
   end type
   type base(l1)
       integer,len   :: l1
       type(dt(l1+1))  :: dtcomp1
   end type

   type,extends(base) :: child(l2)
       integer,len    :: l2
       type(dt(2*l2+1)) :: dtcomp2
   end type

end module

program dummyArgDeferPolyOptional01
  use m
  implicit none

  class(base(:)),allocatable :: poly1
  class(base(:)),pointer     :: poly2=>null()

  type(child(2,3)),target    :: tar

  tar=child(2,3)(dtcomp1=dt(3)("123"),dtcomp2=dt(7)("abcdefg"))

  call sub1(arg1=poly1,arg2=poly2)

  select type(poly1)
     type is(child(*,*))
        print *,poly1%dtcomp1%l,poly1%dtcomp1%c
        print *,poly1%dtcomp2%l,poly1%dtcomp2%c
     class default
        error stop 50_4
  end select

  select type(poly2)
     type is(child(*,*))
        print *,poly2%dtcomp1%l,poly2%dtcomp1%c
        print *,poly2%dtcomp2%l,poly2%dtcomp2%c
     class default
        error stop 51_4
  end select

  call sub1(arg2=poly2)

  select type(poly2)
     type is(child(*,*))
        print *,poly2%dtcomp1%l,poly2%dtcomp1%c
        print *,poly2%dtcomp2%l,poly2%dtcomp2%c
     class default
        error stop 52_4
  end select

  call sub1(arg1=poly1)

  select type(poly1)
     type is(child(*,*))
        print *,poly1%dtcomp1%l,poly1%dtcomp1%c
        print *,poly1%dtcomp2%l,poly1%dtcomp2%c
     class default
        error stop 53_4
  end select

  call sub1()

  contains

     subroutine sub1(arg1,arg2)
        class(base(:)),allocatable,optional,intent(out) :: arg1
        class(base(:)),pointer,optional,intent(out)     :: arg2

        call sub2(arg1,arg2)

     end subroutine

     subroutine sub2(arg1,arg2)
        class(base(:)),allocatable,optional,intent(out) :: arg1
        class(base(:)),pointer,optional,intent(out)     :: arg2

        if(present(arg1)) then

           allocate(arg1,source=child(1,2)(dtcomp1=dt(2)("01"), &
                            dtcomp2=dt(5)("23456") ) )
        else
           print *,"arg1 is not present"
        end if

        if(present(arg2)) then
           arg2=>tar
        else
           print *,"arg2 is not present"
        end if

     end subroutine


end program
