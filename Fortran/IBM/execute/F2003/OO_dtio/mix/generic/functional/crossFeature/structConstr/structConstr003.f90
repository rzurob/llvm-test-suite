!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 04/26/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Structure Constructor
!*                                    -  Structure constructor with pointer/allocatable components formatted i/o
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
      character(3) :: c
      contains
         procedure, pass :: write => writeb
         generic :: write(formatted) => write
   end type

   type, extends(base) :: child
      integer(4), pointer :: i => null()
      contains
         procedure, pass :: write => writec
   end type

   type, extends(child) :: gen3
      character(3), pointer :: s => null()
      contains
         procedure, pass :: write => writeg
   end type

   contains

      subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt
         write (fmt, *) "(A", v_list(1), ")"

         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt
         write (fmt, *) "(A", v_list(1), ",I",v_list(2),")"

         if ( .not. associated( dtv%i ) ) then
            write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c, 9999
         else
            write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         end if

         iomsg = 'dtiowritec'

      end subroutine

      subroutine writeg (dtv, unit, iotype, v_list, iostat, iomsg)
         class(gen3), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(30) :: fmt1, fmt2
         write (fmt1, *) "(A", v_list(1), ", I",v_list(2)," )"
         write (fmt2, *) "(A", v_list(3),")"

         if ( .not. associated( dtv%i ) ) then
            write (unit, fmt1, iostat=iostat, iomsg=iomsg) dtv%c, 9999
         else
            write (unit, fmt1, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         end if

         if ( associated( dtv%s ) ) then
            write (unit, fmt2, iostat=iostat, iomsg=iomsg)     dtv%s
         else
            write (unit, fmt2, iostat=iostat, iomsg=iomsg) 'zzz'
         end if

         iomsg = 'dtiowriteg'

      end subroutine

end module

program structConstr003
   use m

   integer :: stat
   character(200) :: msg

   integer, target :: OneThousand    = 1000
   integer, target :: OneThousandOne = 1001
   character(3), target :: zoo = 'ZOO'

   open ( 1, file = 'structConstr003.1', form='formatted', access='sequential' )

   write ( 1, "(DT(3))", iostat = stat, iomsg = msg )        base('abc')
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )         error stop 1_4

   write ( 1, "(DT(3,5))", iostat = stat, iomsg = msg )      child('def')
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )         error stop 2_4

   write ( 1, "(DT(3,5,4))", iostat = stat, iomsg = msg )    gen3('ghi')
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) )         error stop 3_4

   write ( 1, "(DT(3,5))", iostat = stat, iomsg = msg )      child('jkl', OneThousand )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )         error stop 4_4

   write ( 1, "(DT(3,5,4))", iostat = stat, iomsg = msg )    gen3('mno', OneThousandOne, zoo )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) )         error stop 5_4
   
   write ( 1, "(DT(5,5,5))", iostat = stat, iomsg = msg )   ( child (zoo,OneThousand), gen3('cat',OneThousandOne, zoo),i=-1,1 )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) )         error stop 6_4

end program
