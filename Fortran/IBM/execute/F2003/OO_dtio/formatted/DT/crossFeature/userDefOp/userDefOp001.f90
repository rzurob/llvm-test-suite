!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 21/03/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        User Defined Operator: ensure using DT Edit descriptor can invoke DTIO inside
!*                                                               user defined operator procedures
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base
      integer(4) :: i
   end type

   type, extends(base) :: child
      integer(4) :: j
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
   use m, only: base, child

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

   if ( .not. same_type_as ( a, b ) ) error stop 1_4

   select type ( a )
      type is ( base )
         allocate ( myAdd, source = base ( i=(a%i+b%i) ) )
      type is ( child )
         select type ( b )
            type is ( child )
               allocate ( myAdd, source = child ( i=(a%i+b%i), j=(a%j+b%j) ) )
            class default
               error stop 2_4
         end select
   end select

   write ( 1, "(DT(5,5),' = ',DT(6,6),' + ',DT(7,7))", iostat = stat, iomsg = msg )   myAdd, a, b
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

end function

subroutine myAsgn(a,b)
   use m, only: base, child
   class(base), intent(out) :: a
   class(base), intent(in)  :: b
   integer :: stat
   character(200) :: msg

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
   
   if ( .not. same_type_as ( a, b ) ) error stop 4_4

   select type ( b )
      type is (base)
        a%i = b%i
      type is (child)
        select type ( a )
           type is ( child )
              a%i = b%i
              a%j = b%j
           class default
              error stop 5_4
        end select
   end select
   
   write ( 1, "(DT(5,5),' = ',DT(6,6))", iostat = stat, iomsg = msg )   a, b
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 6_4

end subroutine

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(25) :: fmt

   select type ( dtv )
      type is ( base )
         write ( fmt, "(A2,I1,A1)" ) '(I', v_list(1),')'
         write ( unit, fmt, iostat = iostat )    dtv%i
      type is ( child )
         write ( fmt, "(A2,I1,A2,I1,A1)" ) '(I', v_list(1),',I',v_list(2),')'
         write ( unit, fmt, iostat = iostat )    dtv%i, dtv%j
   end select
   iomsg = 'dtiowrite'

end subroutine
