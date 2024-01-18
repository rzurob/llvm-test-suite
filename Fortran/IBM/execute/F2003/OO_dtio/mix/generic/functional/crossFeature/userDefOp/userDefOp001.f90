!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: GENERIC BINDING
!*                                        User Defined Operator: ensure DTIO can be invoked inside
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
      contains
         procedure, pass :: write => writeb
         generic :: write(formatted) => write
   end type

   type, extends(base) :: child
      integer(4) :: j
      contains
         procedure, pass :: write => writec
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

   contains

   subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
      class(base), intent(in) :: dtv
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in)     :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      character(25) :: fmt

      write ( fmt, "(A2,I1,A1)" ) '(I', v_list(1),')'
      write ( unit, fmt, iostat = iostat )    dtv%i
      iomsg = 'dtiowriteb'

   end subroutine

   subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
      class(child), intent(in) :: dtv
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in)     :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      character(25) :: fmt

      write ( fmt, "(A2,I1,A2,I1,A1)" ) '(I', v_list(1),',I',v_list(2),')'
      write ( unit, fmt, iostat = iostat )    dtv%i, dtv%j

      iomsg = 'dtiowritec'

   end subroutine

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

end function

subroutine myAsgn(a,b)
   use m, only: base, child
   class(base), intent(out) :: a
   class(base), intent(in)  :: b
   integer :: stat
   character(200) :: msg

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

end subroutine


