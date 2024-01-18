!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 10 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* An optional dummy argument that is not present is subject to the following restrictions:
!* (6) If it is an array, it shall not be supplied as an actual argument to an elemental procedure unless an array of the same rank is supplied as an actual argument corresponding to a nonoptional dummy argument of that elemental procedure.
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(l1)
     integer,len     :: l1
     character(l1)   :: c1
  end type

  contains

   subroutine sub1(arg1,arg2)
      type(dtp(:)),allocatable,optional :: arg1(:)
      type(dtp(:)),allocatable          :: arg2(:)

      if (present(arg1)) then
        print *,fun1(arg1,arg2)
      else
        print *, fun1(arg2=arg2)
      end if
      contains
         function fun1(arg1,arg2)
            type(dtp(*)),intent(in),optional :: arg1(:)
            type(dtp(*)),intent(in)          :: arg2(:)
            type(dtp(len(arg2%c1)))          :: fun1(size(arg2))

            if(present(arg1)) then
                if (size(arg1) /= size(arg2)) stop 10
                  fun1%c1=arg1%c1
            else
                  fun1%c1=arg2%c1
            end if
         end function
   end subroutine
end module

program dummyArgDeferNonPolyOptional05
  use m
  implicit none

  type(dtp(:)),allocatable :: dtp1(:)
  type(dtp(:)),allocatable :: dtp2(:)

  allocate(dtp1(2),source=[dtp(3)("xlf"),dtp(3)("xlc")])
  allocate(dtp2(2),source=[dtp(3)("123"),dtp(3)("456")])

  call sub1(dtp1,dtp2)

  call sub1(arg2=dtp2)

end program
