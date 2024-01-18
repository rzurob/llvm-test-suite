! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  User-defined operator and assignment with DTIO and namelist formatting
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m

   type :: base
      integer(4) :: i1
   end type

   type, extends(base) :: child
      integer(4)   :: i2
   end type

   interface operator(+)
      class(base) function myAdd(a,b)
         import base, child
         class(base), intent(in) :: a, b
         allocatable :: myAdd
      end function
   end interface

   interface assignment(=)
      subroutine myAsgn(a,b)
         import base, child
         class(base), intent(out) :: a
         class(base), intent(in)  :: b
      end subroutine
   end interface

   class(base), allocatable :: b1, b2, b3, b4
   namelist /n1/ b1, b2

end module

program userDefOp001
   use m

   open ( 1, file='userDefOp001.1', form='formatted', access='sequential' )

   allocate ( b1, source = child ( 1000, 1001 ) )
   allocate ( b2, source = child ( 2000, 2002 ) )
   allocate ( child :: b3 )

   b3 = b1 + b2

   deallocate ( b1, b2 )

   allocate ( b1, source = base ( 100 ) )
   allocate ( b2, source = base ( 200 ) )
   allocate ( base :: b4 )

   b4 = b1 + b2

end program

class(base) function myAdd(a,b)
   use m, only: base, child, n1

   interface write(formatted)
      subroutine writeFormatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat
   character(200) :: msg
   class(base), intent(in) :: a, b
   allocatable :: myAdd

   namelist /add/ myAdd

   if ( .not. same_type_as ( a, b ) ) error stop 1_4

   select type ( a )
      type is ( base )
         allocate ( myAdd, source = base ( i1= ( a%i1+b%i1 ) ) )
         write ( 1, n1, iostat = stat, iomsg = msg )
         write ( 1, add, iostat = stat, iomsg = msg )
      type is ( child )
         select type ( b )
            type is ( child )
               allocate ( myAdd, source = child ( i1= ( a%i1+b%i1 ), i2= ( a%i2+b%i2 ) ) )
               write ( 1, n1, iostat = stat, iomsg = msg )
               write ( 1, add, iostat = stat, iomsg = msg )
            class default
               error stop 2_4
         end select
   end select

end function

subroutine myAsgn(a,b)
   use m, only: base, child
   class(base), intent(out) :: a
   class(base), intent(in)  :: b

   if ( .not. same_type_as ( a, b ) ) error stop 3_4

   select type ( b )
      type is (base)
        a%i1 = b%i1
      type is (child)
        select type ( a )
           type is ( child )
              a%i1 = b%i1
              a%i2 = b%i2
           class default
              error stop 4_4
        end select
   end select

end subroutine

subroutine writeFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m
   class(base), intent(in) :: dtv
   integer,  intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer,  intent(out) :: iostat
   character(*),  intent(inout) :: iomsg

   select type ( dtv )
      type is (base)
         write (unit, "('i1=',I4)", iostat=iostat, iomsg=iomsg ) dtv%i1
      type is (child)
         write (unit, "('i1=',I4,1X,'i2=',I4)", iostat=iostat, iomsg=iomsg ) dtv%i1, dtv%i2
   end select

   iomsg = 'dtiowrite'

end subroutine
